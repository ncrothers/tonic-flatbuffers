use flatbuffers::FlatBufferBuilder;
use helloworld_example::generated::example::service::{
    hello_world_streaming_server::{HelloWorldStreaming, HelloWorldStreamingServer},
    OwnedRequest, OwnedResponse, Response, ResponseArgs,
};
use tokio::runtime::Runtime;
use tonic::{async_trait, codec::CompressionEncoding};
use tonic_flatbuffers::flatbuffers_owned::RelaxedFlatBufferTrait;

struct TestServer;

#[async_trait]
impl HelloWorldStreaming for TestServer {
    async fn hello_single(
        &self,
        request: tonic::Request<OwnedRequest>,
    ) -> std::result::Result<tonic::Response<OwnedResponse>, tonic::Status> {
        let req = request.into_inner();

        let data = req.as_actual();

        println!("Got request with data: {}", data.foo());
        println!("Got request with data: {}", data.bar().unwrap());

        let mut builder = FlatBufferBuilder::new();

        let bar_str = builder.create_string(data.bar().unwrap());

        let resp = Response::create(
            &mut builder,
            &ResponseArgs {
                foo: data.foo(),
                bar: Some(bar_str),
            },
        );
        builder.finish_minimal(resp);

        let resp = builder.finished_data();

        let mut resp_full = Vec::new();
        resp_full.push(0u8);
        resp_full.extend((resp.len() as u32).to_be_bytes());
        resp_full.extend(resp);

        println!("Sending response");

        Ok(tonic::Response::new(
            OwnedResponse::new(Box::from(resp)).unwrap(),
        ))
    }
}

fn main() {
    let rt = Runtime::new().unwrap();

    rt.block_on(async move {
        tonic::transport::Server::builder()
            .add_service(
                HelloWorldStreamingServer::new(TestServer)
                    .accept_compressed(CompressionEncoding::Gzip),
            )
            .serve("0.0.0.0:8081".parse().unwrap())
            .await
            .unwrap();
    });
}

use flatbuffers::FlatBufferBuilder;
use helloworld_example::generated::example::service::{
    hello_world_streaming_client::HelloWorldStreamingClient, OwnedRequest, Request, RequestArgs,
};
use tokio::runtime::Runtime;
use tonic::codec::CompressionEncoding;
use tonic_flatbuffers::flatbuffers_owned::RelaxedFlatBufferTrait;

fn main() {
    let rt = Runtime::new().unwrap();

    rt.block_on(async move {
        let client = HelloWorldStreamingClient::connect("http://localhost:8081")
            .await
            .unwrap();

        let mut client = client.send_compressed(CompressionEncoding::Gzip);

        let mut builder = FlatBufferBuilder::new();

        let bar_str = builder.create_string("hello world");

        let req = Request::create(
            &mut builder,
            &RequestArgs {
                foo: 123,
                bar: Some(bar_str),
            },
        );
        builder.finish_minimal(req);

        let req = builder.finished_data();

        let req = OwnedRequest::new(Box::from(req)).unwrap();

        let resp = client.hello_single(req).await.unwrap();

        let resp = resp.into_inner();
        let resp = resp.as_actual();

        println!("Response: {}", resp.foo());
        println!("Response: {}", resp.bar().unwrap());
    });
}

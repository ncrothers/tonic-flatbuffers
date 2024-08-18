use std::marker::PhantomData;

use crate::flatbuffers_owned::RelaxedFlatBufferTrait;
use bytes::{Buf, BufMut};
use tonic::{
    codec::{BufferSettings, Codec, Decoder, Encoder},
    Status,
};

impl<T> Message for T
where
    T: RelaxedFlatBufferTrait<Box<[u8]>>,
{
    fn decode(buf: &[u8]) -> Result<Self, std::io::Error> {
        // TODO: Don't do it unchecked!
        <T as RelaxedFlatBufferTrait<Box<[u8]>>>::new(Box::from(buf))
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
    }
}

pub trait Message: RelaxedFlatBufferTrait<Box<[u8]>> {
    fn decode(buf: &[u8]) -> Result<Self, std::io::Error>;
}

pub struct FlatCodec<T, U> {
    _pd: PhantomData<(T, U)>,
}

impl<T, U> FlatCodec<T, U> {
    pub fn new() -> Self {
        Self { _pd: PhantomData }
    }
}

impl<T, U> Default for FlatCodec<T, U> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, U> FlatCodec<T, U>
where
    T: Message + Send + 'static,
    U: Message + Send + 'static,
{
    /// A tool for building custom codecs based on prost encoding and decoding.
    /// See the codec_buffers example for one possible way to use this.
    pub fn raw_encoder(buffer_settings: BufferSettings) -> <Self as Codec>::Encoder {
        FlatEncoder {
            _pd: PhantomData,
            buffer_settings,
        }
    }

    /// A tool for building custom codecs based on prost encoding and decoding.
    /// See the codec_buffers example for one possible way to use this.
    pub fn raw_decoder(buffer_settings: BufferSettings) -> <Self as Codec>::Decoder {
        FlatDecoder {
            _pd: PhantomData,
            buffer_settings,
        }
    }
}

impl<T, U> Codec for FlatCodec<T, U>
where
    T: Message + Send + 'static,
    U: Message + Send + 'static,
{
    type Encode = T;
    type Decode = U;

    type Encoder = FlatEncoder<T>;
    type Decoder = FlatDecoder<U>;

    fn encoder(&mut self) -> Self::Encoder {
        FlatEncoder {
            _pd: PhantomData,
            buffer_settings: BufferSettings::default(),
        }
    }

    fn decoder(&mut self) -> Self::Decoder {
        FlatDecoder {
            _pd: PhantomData,
            buffer_settings: BufferSettings::default(),
        }
    }
}

pub struct FlatEncoder<T> {
    _pd: PhantomData<T>,
    buffer_settings: BufferSettings,
}

impl<T> Encoder for FlatEncoder<T>
where
    T: Message + Send + 'static,
{
    type Item = T;

    type Error = Status;

    fn encode(
        &mut self,
        item: Self::Item,
        dst: &mut tonic::codec::EncodeBuf<'_>,
    ) -> Result<(), Self::Error> {
        dst.put(&*item);

        Ok(())
    }

    fn buffer_settings(&self) -> BufferSettings {
        self.buffer_settings
    }
}

pub struct FlatDecoder<U> {
    _pd: PhantomData<U>,
    buffer_settings: BufferSettings,
}

impl<U: Message> Decoder for FlatDecoder<U> {
    type Item = U;

    type Error = Status;

    fn decode(
        &mut self,
        src: &mut tonic::codec::DecodeBuf<'_>,
    ) -> Result<Option<Self::Item>, Self::Error> {
        // println!("Decode buffer: {:?}", src.chunk());
        let remaining = src.remaining();
        if remaining > 0 {
            let data = Message::decode(src.chunk()).map_err(|e| {
                Status::new(
                    tonic::Code::Internal,
                    format!("Error decoding message: {e}"),
                )
            })?;
            src.advance(remaining);
            Ok(Some(data))
        } else {
            Ok(None)
        }
    }

    fn buffer_settings(&self) -> BufferSettings {
        self.buffer_settings
    }
}

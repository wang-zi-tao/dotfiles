//! 隧道流分帧：2 字节大端长度前缀 + 载荷。

use bytes::{Buf, Bytes, BytesMut};

/// 单帧最大载荷（与 u16 长度前缀一致）。
pub const MAX_FRAME: usize = u16::MAX as usize;

/// 把一帧写入缓冲区。
pub fn encode_frame(payload: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(2 + payload.len());
    out.extend_from_slice(&(payload.len() as u16).to_be_bytes());
    out.extend_from_slice(payload);
    out
}

/// 增量拆帧器。
#[derive(Default)]
pub struct FrameReader {
    buf: BytesMut,
}

impl FrameReader {
    pub fn new() -> Self {
        Self::default()
    }

    /// 喂入一段数据。
    pub fn feed(&mut self, chunk: Bytes) {
        self.buf.extend_from_slice(&chunk);
    }

    /// 取出下一个完整帧；数据不足返回 None。
    pub fn next_frame(&mut self) -> Option<Vec<u8>> {
        if self.buf.len() < 2 {
            return None;
        }
        let len = u16::from_be_bytes([self.buf[0], self.buf[1]]) as usize;
        if self.buf.len() < 2 + len {
            return None;
        }
        self.buf.advance(2);
        Some(self.buf.split_to(len).to_vec())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_and_partial() {
        let mut reader = FrameReader::new();
        let all = [encode_frame(b"hello"), encode_frame(b"world")].concat();
        reader.feed(Bytes::from(all[..4].to_vec()));
        assert!(reader.next_frame().is_none());
        reader.feed(Bytes::from(all[4..].to_vec()));
        assert_eq!(reader.next_frame().unwrap(), b"hello");
        assert_eq!(reader.next_frame().unwrap(), b"world");
        assert!(reader.next_frame().is_none());
    }

    #[test]
    fn empty_frame_is_valid() {
        let mut reader = FrameReader::new();
        reader.feed(Bytes::from(encode_frame(b"")));
        assert_eq!(reader.next_frame().unwrap(), Vec::<u8>::new());
    }
}

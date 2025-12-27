; JSON encoding and decoding
module json {
  #[builtin]
  ; Encodes a value into a JSON string.
  fn encode(value: any): str { builtin::encoding::json_encode(value) }

  #[builtin]
  ; Decodes a JSON string into a value.
  fn decode(text: str): any { builtin::encoding::json_decode(text) }
}

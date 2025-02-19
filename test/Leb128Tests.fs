module TinyFS.Test.Leb128Tests

open TinyFS.Core.AstToWasm
open Xunit

let getUnsignedExpectedBytes (num: uint32) =
    match num with
    | 2u -> [| 2uy |]
    | 64u -> [| 64uy |]
    | 127u -> [| 127uy |]
    | 128u -> [| 128uy; 1uy |]
    | 16383u -> [| 255uy; 127uy |]
    | 16384u -> [| 128uy; 128uy; 1uy |]
    | 283828u -> [| 180uy; 169uy; 17uy |]
    | 4_294_967_295u -> [| 255uy; 255uy; 255uy; 255uy; 15uy |]
    | _ -> [||]

[<Theory>]
[<InlineData(2u, 1)>]
[<InlineData(64u, 1)>]
[<InlineData(127u, 1)>]
[<InlineData(128u, 2)>]
[<InlineData(16383u, 2)>]
[<InlineData(16384u, 3)>]
[<InlineData(283828u, 3)>]
[<InlineData(4_294_967_295u, 5)>]
let ``Can encode unsigned integer via LEB128`` num expectedLength =
    let lebEncoded = u32 num

    Assert.Equal(expectedLength, lebEncoded.Length)

    let expectedBytes = getUnsignedExpectedBytes num

    if expectedLength = 5 then
        Assert.Equal(expectedBytes[4], lebEncoded[4])
        Assert.Equal(expectedBytes[3], lebEncoded[3])
        Assert.Equal(expectedBytes[2], lebEncoded[2])
        Assert.Equal(expectedBytes[1], lebEncoded[1])
        Assert.Equal(expectedBytes[0], lebEncoded[0])
    elif expectedLength = 3 then
        Assert.Equal(expectedBytes[2], lebEncoded[2])
        Assert.Equal(expectedBytes[1], lebEncoded[1])
        Assert.Equal(expectedBytes[0], lebEncoded[0])

    elif expectedLength = 2 then
        Assert.Equal(expectedBytes[1], lebEncoded[1])
        Assert.Equal(expectedBytes[0], lebEncoded[0])
    else
        Assert.Equal(expectedBytes[0], lebEncoded[0])

let getSignedExpectedBytes (num: int32) =
    match num with
    | 1 -> [| 1uy |]
    | -1 -> [| 127uy |]
    | 63 -> [| 63uy |]
    | 64 -> [| 192uy; 0uy |]
    | -64 -> [| 64uy |]
    | -65 -> [| 191uy; 127uy |]
    | 127 -> [| 255uy; 0uy |]
    | 128 -> [| 128uy; 1uy |]
    | -128 -> [| 128uy; 127uy |]
    | -129 -> [| 255uy; 126uy |]
    | 7196 -> [| 156uy; 56uy |]
    | 8192 -> [| 128uy; 192uy; 0uy |]
    | -7196 -> [| 228uy; 71uy |]
    | -8193 -> [| 255uy; 191uy; 127uy |]
    | 283828 -> System.Convert.FromHexString("B4A911")
    | 2_147_483_647 -> System.Convert.FromHexString("FFFFFFFF07")
    | -283828 -> System.Convert.FromHexString("CCD66E")
    | -2_147_483_648 -> System.Convert.FromHexString("8080808078")
    | _ -> [||]

[<Theory>]
[<InlineData(1)>]
[<InlineData(-1)>]
[<InlineData(63)>]
[<InlineData(64)>]
[<InlineData(-64)>]
[<InlineData(-65)>]
[<InlineData(127)>]
[<InlineData(128)>]
[<InlineData(-128)>]
[<InlineData(-129)>]
[<InlineData(7196)>]
[<InlineData(8192)>]
[<InlineData(-7196)>]
[<InlineData(-8193)>]
[<InlineData(283828)>]
[<InlineData(2_147_483_647)>]
[<InlineData(-283828)>]
[<InlineData(-2_147_483_648)>]
let ``Can encode signed integer via LEB128`` num =
    let lebEncoded = i32 num
    let expectedBytes = getSignedExpectedBytes num

    Assert.Equal(expectedBytes.Length, lebEncoded.Length)

    let expectedLength = expectedBytes.Length

    if expectedLength = 5 then
        Assert.Equal(expectedBytes[4], lebEncoded[4])
        Assert.Equal(expectedBytes[3], lebEncoded[3])
        Assert.Equal(expectedBytes[2], lebEncoded[2])
        Assert.Equal(expectedBytes[1], lebEncoded[1])
        Assert.Equal(expectedBytes[0], lebEncoded[0])
    elif expectedLength = 3 then
        Assert.Equal(expectedBytes[2], lebEncoded[2])
        Assert.Equal(expectedBytes[1], lebEncoded[1])
        Assert.Equal(expectedBytes[0], lebEncoded[0])

    elif expectedLength = 2 then
        Assert.Equal(expectedBytes[1], lebEncoded[1])
        Assert.Equal(expectedBytes[0], lebEncoded[0])
    else
        Assert.Equal(expectedBytes[0], lebEncoded[0])
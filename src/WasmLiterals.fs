module TinyFS.Core.WasmLiterals

//https://webassembly.github.io/spec/core/appendix/index-instructions.html
module Section =
    [<Literal>]
    let SECTION_ID_TYPE = 0x1uy

    [<Literal>]
    let SECTION_ID_FUNCTION = 0x3uy

    [<Literal>]
    let SECTION_ID_EXPORT = 0x7uy

    [<Literal>]
    let SECTION_ID_CODE = 0xAuy

    [<Literal>]
    let TYPE_FUNCTION = 0x60uy

    [<Literal>]
    let f64_VAL_TYPE = 0x7Cuy

    [<Literal>]
    let f32_VAL_TYPE = 0x7Duy

    [<Literal>]
    let i64_VAL_TYPE = 0x7Euy

    [<Literal>]
    let i32_VAL_TYPE = 0x7Fuy

module Instructions =
    [<Literal>]
    let INSTR_BLOCK = 0x2uy

    [<Literal>]
    let INSTR_LOOP = 0x3uy

    [<Literal>]
    let INSTR_IF = 0x4uy

    [<Literal>]
    let INSTR_ELSE = 0x5uy

    [<Literal>]
    let INSTR_END = 0xBuy

    [<Literal>]
    let INSTR_BR = 0xCuy

    [<Literal>]
    let INSTR_BR_IF = 0xDuy

    [<Literal>]
    let INSTR_CALL = 0x10uy

    [<Literal>]
    let INSTR_DROP = 0x1Auy

    [<Literal>]
    let INSTR_LOCAL_GET = 0x20uy

    [<Literal>]
    let INSTR_LOCAL_SET = 0x21uy

    [<Literal>]
    let INSTR_LOCAL_TEE = 0x22uy

    [<Literal>]
    let EMPTY = 0x40uy

    [<Literal>]
    let i32_CONST = 0x41uy

    [<Literal>]
    let i64_CONST = 0x42uy

    [<Literal>]
    let f32_CONST = 0x43uy

    [<Literal>]
    let f64_CONST = 0x44uy

    [<Literal>]
    let INSTR_i32_EQZ = 0x45uy

    [<Literal>]
    let INSTR_i32_EQ = 0x46uy

    [<Literal>]
    let INSTR_i32_NE = 0x47uy

    [<Literal>]
    let INSTR_i32_LT_S = 0x48uy

    [<Literal>]
    let INSTR_i32_LT_U = 0x49uy

    [<Literal>]
    let INSTR_i32_GT_S = 0x4Auy

    [<Literal>]
    let INSTR_i32_GT_U = 0x4Buy

    [<Literal>]
    let INSTR_i32_LE_S = 0x4Cuy

    [<Literal>]
    let INSTR_i32_LE_U = 0x4Duy

    [<Literal>]
    let INSTR_i32_GE_S = 0x4Euy

    [<Literal>]
    let INSTR_i32_GE_U = 0x4Fuy

    [<Literal>]
    let INSTR_i64_EQ = 0x51uy

    [<Literal>]
    let INSTR_i64_NE = 0x52uy

    [<Literal>]
    let INSTR_i64_LT_S = 0x53uy

    [<Literal>]
    let INSTR_i64_LT_U = 0x54uy

    [<Literal>]
    let INSTR_i64_GT_S = 0x55uy

    [<Literal>]
    let INSTR_i64_GT_U = 0x56uy

    [<Literal>]
    let INSTR_i64_LE_S = 0x57uy

    [<Literal>]
    let INSTR_i64_LE_U = 0x58uy

    [<Literal>]
    let INSTR_i64_GE_S = 0x59uy

    [<Literal>]
    let INSTR_i64_GE_U = 0x5Auy

    [<Literal>]
    let INSTR_f32_EQ = 0x5Buy

    [<Literal>]
    let INSTR_f32_NE = 0x5Cuy

    [<Literal>]
    let INSTR_f32_LT = 0x5Duy

    [<Literal>]
    let INSTR_f32_GT = 0x5Euy

    [<Literal>]
    let INSTR_f32_LE = 0x5Fuy

    [<Literal>]
    let INSTR_f32_GE = 0x60uy

    [<Literal>]
    let INSTR_f64_EQ = 0x61uy

    [<Literal>]
    let INSTR_f64_NE = 0x62uy

    [<Literal>]
    let INSTR_f64_LT = 0x63uy

    [<Literal>]
    let INSTR_f64_GT = 0x64uy

    [<Literal>]
    let INSTR_f64_LE = 0x65uy

    [<Literal>]
    let INSTR_f64_GE = 0x66uy

    [<Literal>]
    let INSTR_i32_ADD = 0x6Auy

    [<Literal>]
    let INSTR_i32_SUB = 0x6Buy

    [<Literal>]
    let INSTR_i32_MUL = 0x6Cuy

    [<Literal>]
    let INSTR_i32_DIV_S = 0x6Duy

    [<Literal>]
    let INSTR_i32_DIV_U = 0x6Euy

    [<Literal>]
    let INSTR_i32_MOD_S = 0x6Fuy

    [<Literal>]
    let INSTR_i32_MOD_U = 0x70uy

    [<Literal>]
    let INSTR_i64_ADD = 0x7Cuy

    [<Literal>]
    let INSTR_i64_SUB = 0x7Duy

    [<Literal>]
    let INSTR_i64_MUL = 0x7Euy

    [<Literal>]
    let INSTR_i64_DIV_S = 0x7Fuy

    [<Literal>]
    let INSTR_i64_DIV_U = 0x80uy

    [<Literal>]
    let INSTR_i64_MOD_S = 0x81uy

    [<Literal>]
    let INSTR_i64_MOD_U = 0x82uy

    [<Literal>]
    let INSTR_f32_ADD = 0x92uy

    [<Literal>]
    let INSTR_f32_SUB = 0x93uy

    [<Literal>]
    let INSTR_f32_MUL = 0x94uy

    [<Literal>]
    let INSTR_f32_DIV = 0x95uy

    [<Literal>]
    let INSTR_f64_ADD = 0xA0uy

    [<Literal>]
    let INSTR_f64_SUB = 0xA1uy

    [<Literal>]
    let INSTR_f64_MUL = 0xA2uy

    [<Literal>]
    let INSTR_f64_DIV = 0xA3uy

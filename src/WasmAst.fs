module Fado.Core.WasmAst

type Literal =
    | IntLiteral of int32
    | LongLiteral of int64
type BinOp = | Plus
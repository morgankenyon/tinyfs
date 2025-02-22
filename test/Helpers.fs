module TinyFS.Test.Helpers

open Wasmtime

let printWasm (bytes: byte list) =
    let stringRepresentation =
        bytes
        |> List.map (fun by -> by.ToString())
        |> String.concat "\n"
    //let str = stringRepresentation |> String.concat ""
    System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
    System.IO.File.WriteAllBytes("./atest.wasm", List.toArray bytes)

let private buildInstance (wasmBytes: byte list) =
    let engine = new Engine()

    let byteArray = List.toArray wasmBytes
    let modd = Module.FromBytes(engine, "fsharpWasm", byteArray)

    let linker = new Linker(engine)
    let store = new Store(engine)

    linker.Instantiate(store, modd)

let runFuncInt32Return (funcName: string) (wasmBytes: byte list) =
    let instance = buildInstance wasmBytes

    let func = instance.GetFunction<int32>(funcName)
    func.Invoke()

let runFuncInt64Return (funcName: string) (wasmBytes: byte list) =
    let instance = buildInstance wasmBytes

    let func = instance.GetFunction<int64>(funcName)
    func.Invoke()

let runInt32FuncInt32 (funcName: string) (param1: int32) (wasmBytes: byte list) =
    let instance = buildInstance wasmBytes

    let func = instance.GetFunction<int32, int32>(funcName)
    func.Invoke(param1)

let runInt64FuncInt64 (funcName: string) (param1: int64) (wasmBytes: byte list) =
    let instance = buildInstance wasmBytes

    let func = instance.GetFunction<int64, int64>(funcName)
    func.Invoke(param1)

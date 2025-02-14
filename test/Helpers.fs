module TinyFS.Test.Helpers

open Fable
open TinyFS.Test.TestObjs
open Wasmtime



let printWasm (bytes: byte array) =
    let stringRepresentation = bytes
                                |> Array.map (fun by -> by.ToString())
                                |> String.concat ""
    //let str = stringRepresentation |> String.concat ""
    System.IO.File.WriteAllText("./atest.txt", stringRepresentation)
    System.IO.File.WriteAllBytes("./atest.wasm", bytes)

let private buildInstance (wasmBytes: byte array) =
    let engine = new Engine()

    let modd = Module.FromBytes(engine, "fsharpWasm", wasmBytes)

    let linker = new Linker(engine)
    let store = new Store(engine)

    linker.Instantiate(store, modd)

let runFuncInt32Return (funcName: string) (wasmBytes: byte array) =
    let instance = buildInstance wasmBytes

    let func = instance.GetFunction<int32>(funcName)
    func.Invoke()

let makeCompiler (input: string) =
    let compilerOptions = CompilerOptionsHelper.Make()
    let compiler = TestCompiler("test", compilerOptions, input)
    compiler
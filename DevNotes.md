# Developer Notes

## Error

```
Microsoft.FSharp.Core.Operators{"Method not found: 'Boolean FSharp.Compiler.Symbols.FSharpGenericParameterConstraint.get_IsAllowsRefStructConstraint()'."}
```

I fixed this by pulling in the FSharp.Compiler.Service dlls that Fable uses, since they are slight tweaks from what is in Nuget itself.

## WebAssembly Links

* https://webassembly.github.io/wabt/demo/wasm2wat/
* https://webassembly.github.io/wabt/demo/wat2wasm/
* https://wasdk.github.io/wasmcodeexplorer/
* https://webassembly.github.io/spec/core/appendix/index-instructions.html
* https://github.com/vshymanskyy/awesome-wasm-tools/tree/main
# Developer Notes

## Error

```
Microsoft.FSharp.Core.Operators{"Method not found: 'Boolean FSharp.Compiler.Symbols.FSharpGenericParameterConstraint.get_IsAllowsRefStructConstraint()'."}
```

I fixed this by pulling in the FSharp.Compiler.Service dlls that Fable uses, since they are slight tweaks from what is in Nuget itself.
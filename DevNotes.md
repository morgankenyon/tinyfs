# Developer Notes

## Tagging a Release

```
git tag -a v0.0.1 -m "First release of TinyFS"
git push origin tag v0.0.1
```
## Error

```
Microsoft.FSharp.Core.Operators{"Method not found: 'Boolean FSharp.Compiler.Symbols.FSharpGenericParameterConstraint.get_IsAllowsRefStructConstraint()'."}
```

I fixed this by pulling in the FSharp.Compiler.Service dlls that Fable uses, since they are slight tweaks from what is in Nuget itself.
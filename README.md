# purescript-dispatcher-react

A simple action dispatcher for `purescript-react`.

## Goals

* Facilitate writing React components which are black boxes - see this [comparison](comparison.md) with
  [Thermite](https://github.com/paf31/purescript-thermite) and [Halogen](https://github.com/slamdata/purescript-halogen).
* Make it easy to use purescript wrapper libraries which model React properties as purescript
  records which directly correspond to what the library expects. In particular many React libraries have event callbacks
  which are best modeled as `EffFnX` purescript functions.
* Always try and keep the code you write "type inferable". Being forced to enter a long type signature is never fun.



## Example

`bower install purescript-simpleaction-react --save`

#!/usr/bin/env stack
{-
  stack --resolver lts-11.3 script
    --package lens
-}

-- -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-import-lists

--
-- Walking through
-- <https://functor.tokyo/blog/2017-07-28-ghc-warnings-you-should-enable>
-- and
-- <https://gist.github.com/cdepillabout/af414dcb9032a29e1ec4562edd7209d3>.
--

import Control.Lens
  (Prism', Traversal', (^.), _1, _2, preview, prism', set)

data Foo
  = Bar Int String
  | Baz
  deriving (Show)

_Bar :: Prism' Foo (Int, String)
_Bar = prism'
  (\(int, string) -> Bar int string)
  (\foo ->
    case foo of
      Baz -> Nothing
      Bar int string -> Just (int, string))

_Baz :: Prism' Foo ()
_Baz = prism'
  (\() -> Baz)
  (\foo ->
    case foo of
      Baz -> Just ()
      Bar _ _ -> Nothing)

barIntTraversal :: Traversal' Foo Int
barIntTraversal = _Bar . _1

barStringTraversal :: Traversal' Foo String
barStringTraversal = _Bar . _2

main = do
  let bar = Bar 3 "Hello"
  print bar
  print (preview barIntTraversal bar)
  print (set barIntTraversal 6 bar)
  let baz = Baz
  print baz
  print (preview _Bar bar)
  print (preview _Bar baz)

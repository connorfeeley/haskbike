-- |

module Test where

newtype NoRunNetworkOpt = NoRunNetworkOpt Switch
  deriving stock (Show, Eq)

data Switch = Unset | Set
  deriving stock (Show, Eq)

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Tasty.Option $ Proxy @NoRunNetworkOpt] : defaultIngredients)
    tree

tree :: TestTree
tree =
  askOption @NoRunNetworkOpt $ \case
    NoRunNetworkOpt Unset ->
      testGroup "group1"
        if unsafePerformIO (putStrLn "unsafe things go here" >> pure True)
          then [...]
          else []
    NoRunNetworkOpt Set ->
      -- Can't run IO here!
      testGroup "group1" []

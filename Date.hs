data Date = Date Int Int Int

instance Show Date where
  show (Date a b c) = (show a) ++ "/" ++ (show b) ++ "/" ++ (show c)

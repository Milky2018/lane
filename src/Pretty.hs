module Pretty (Pretty(pretty)) where 

class Pretty a where 
  pretty :: a -> String

instance Pretty () where 
  pretty _ = "()"

instance Pretty t => Pretty (Maybe t) where 
  pretty Nothing = "?"
  pretty (Just t) = pretty t

instance (Pretty a, Pretty b) => Pretty (a, b) where 
  pretty (a, b) = "(" ++ pretty a ++ "," ++ pretty b ++ ")"
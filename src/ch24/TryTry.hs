module TryTry where

import Control.Applicative
import Text.Trifecta

type IntegerOrDouble = Either Integer Double

integerOrDouble' :: Parser IntegerOrDouble
integerOrDouble' = try (Right <$> double)
               <|> try (Left  <$> integer)

type NumberOrString = Either Integer String
parseNos :: Parser NumberOrString
parseNos = Left <$> integer
       <|> Right <$> some letter

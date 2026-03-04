applyMaybeÂ ::Â MaybeÂ aÂ -> (aÂ ->Â MaybeÂ b)Â ->Â MaybeÂ bÂ Â 
applyMaybeÂ NothingÂ fÂ Â =Â NothingÂ Â 
applyMaybeÂ (JustÂ x)Â fÂ =Â fÂ x

test1 = Just 3 `applyMaybe` (\x -> Just $ x * 2) `applyMaybe` (\x -> Just $ x - 1)

test2 = Just 3 `applyMaybe` (\_ -> Nothing) `applyMaybe` (\x -> Just $ x - 1)
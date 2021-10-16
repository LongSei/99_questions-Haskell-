data NestedList a = Elem a | List [NestedList a]
flattern :: NestedList a -> [a]
flattern (List []) = []
flattern (Elem a) = [a]
flattern (List (x:xs)) = flattern x ++ flattern (List xs)

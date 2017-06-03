data Tree a = NullTree | Tree a ( Tree a ) ( Tree a ) 
		deriving ( Show, Read, Eq )

-- A shortcut for creating a Tree.
singleton :: a -> Tree a
singleton x = Tree x NullTree NullTree

-- Insert into the Binary Tree.
treeInsert :: ( Ord a ) => a -> Tree a -> Tree a
treeInsert x NullTree = singleton x
treeInsert x ( Tree a l r )
	| x == a = Tree x l r
	| x < a = Tree a ( treeInsert x l ) r
	| x > a = Tree a l ( treeInsert x r )
	
-- Determine weather or not a value is in the BT.
treeElem :: ( Ord a ) => a -> Tree a -> Bool
treeElem x NullTree = False
treeElem x ( Tree a l r )
	| x == a = True
	| x < a = treeElem x l
	| x > a = treeElem x r
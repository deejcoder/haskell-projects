infixr 5 :->
data List a = Null | a :-> ( List a ) 
		deriving (Show, Read, Eq, Ord)  


infixr 5 +++
(+++) :: List a -> List a -> List a   
Null +++ ys = ys  
(x :-> xs) +++ ys = x :-> (xs +++ ys )


import Data.List
import System.IO

data Text = Text [Char] [Char] [Char] Int
	deriving Show
	
a = (Text "left here" " right here" "copied text" 5)

create :: Text
create = Text [] [] [] 0

initialise :: Text -> Text
initialise (Text l r d s) = Text l r d s

addChar :: Text -> Char -> Text
addChar (Text l r d s) a = if (length (l ++ r) < 500) then ad (Text l r d s) a else Text l r d s
	where
		ad (Text l r d 0) a = Text (reverse (a:(reverse l))) r d 0
		ad (Text l r d s) a = ad (deleteSelected (Text l r d s)) a

deleteSelected :: Text -> Text
deleteSelected (Text l r d s) = de (Text l r d s)
	where
		de (Text l r d 0) = (Text l r d 0)
		de (Text l (r:rr) d s) = if (s > 0) then de (Text l rr d (s - 1))
			else de (Text (init l) (r:rr) d (s + 1))

left :: Text -> Text
left (Text l r d _) = Text (init l) (last l : r) d 0

right :: Text -> Text
right (Text l (r:rr) d _) = Text (reverse (r : reverse l)) rr d 0

leftSelect :: Text -> Text
leftSelect (Text l r d s) = Text (init l) (last l : r) d (s + 1)

rightSelect :: Text -> Text
rightSelect (Text l (r:rr) d s) = Text (reverse (r : reverse l)) rr d (s - 1)

rightSpace :: Text -> Text
rightSpace (Text l r d _) = findRight (Text l r d 0) 0
	where
		findRight (Text l [] d s) i = Text l [] d 0
		findRight (Text l (r:rr) d s) i = if ((i > 0) && (last l == ' ') && (r /= ' ')) then (Text l (r:rr) d s)
			else findRight (Text (reverse (r :(reverse l))) rr d s) 1

leftSpace :: Text -> Text
leftSpace (Text l r d _) = findLeft (Text l r d 0) 0
	where
		findLeft (Text [] r d s) i = (Text [] r d s)
		findLeft (Text l right@(r:rr) d s) i = if ((i > 0) && (last l == ' ') && (r /= ' ')) then Text l right d s
			else findLeft (Text (init l) ((last l) : right) d s) 1

leftSpaceSelect :: Text -> Text
leftSpaceSelect (Text l r d s) = findLeftSelect (Text l r d s ) 0
	where
		findLeftSelect (Text [] r d s) i = Text [] r d s
		findLeftSelect (Text l (r:rr) d s) i = if ((i > 0) && (last l == ' ') && (r /= ' ')) then Text l (r:rr) d s
			else findLeftSelect (Text (init l) ((last l):(r:rr)) d (s + 1)) 1

rightSpaceSelect :: Text -> Text
rightSpaceSelect (Text l r d s) = findRightSelect (Text l r d s) 0
	where
		findRightSelect (Text l [] d s) i = Text l [] d s
		findRightSelect (Text l (r:rr) d s) i = if ((i > 0) && (last l == ' ') && (r /= ' ')) then (Text l (r:rr) d s)
			else findRightSelect (Text (reverse (r :(reverse l))) rr d (s - 1)) 1
			
start :: Text -> Text
start (Text l r d _) = Text [] (l ++ r) d 0

end :: Text -> Text
end (Text l r d _) = Text (l ++ r) [] d 0

startSelect :: Text -> Text
startSelect (Text l r d s) = findStart (Text l r d s)
	where
		findStart (Text [] r d s) = Text [] r d s
		findStart (Text l r d s) = findStart (Text (init l) ((last l):r) d (s+1))

endSelect :: Text -> Text
endSelect (Text l r d s) = findEnd (Text l r d s)
	where
		findEnd (Text l [] d s) = Text l [] d s
		findEnd (Text l (r:rr) d s) = findEnd (Text (reverse (r :(reverse l))) rr d (s - 1))

selectAll :: Text -> Text
selectAll (Text l r d s) = (Text [] (l ++ r) d (length (l ++ r)))

copy :: Text -> Text
copy (Text l r _ s) = copyAllCheck (Text l r [] s) 0
	where
		copyAllCheck (Text l r d s) i = if (i == s) then (Text l r d (s * (-1))) else (copyAllGo (Text l r d s) i)
		copyAllGo (Text l r d s) i = if (s > 0) then copyAllCheck (Text (reverse ((head r) : reverse l)) (tail r) (reverse ((head r):(reverse d))) s) (i + 1)
			else copyAllCheck (Text (init l) (last l : r) ((last l):(d)) s) (i - 1)
			
cut :: Text -> Text
cut (Text l r d s) = deleteSelected (copy (Text l r d s))

paste :: Text -> Text
paste (Text l r d s) = if (s == 0) then (Text (l ++ d) r d 0) else paste(deleteSelected (Text l r d s))

del :: Text -> Text
del (Text l [] d s) = deleteSelected (Text l [] d s)
del (Text l r d 0) = Text l (tail r) d 0
del (Text l r d s) = deleteSelected (Text l r d s)

backspace :: Text -> Text
backspace (Text [] r d s) = deleteSelected (Text [] r d s)
backspace (Text l r d 0) = Text (init l) r d 0
backspace (Text l r d s) = deleteSelected (Text l r d s)
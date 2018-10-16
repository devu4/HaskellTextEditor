data TextEditor = TextEditor String String String String deriving (Show) -- essentially creating the create function of custom data type. 

-- delete after cursor function
delete :: TextEditor -> TextEditor
delete(TextEditor q s a b) =
    if s == [] -- if nothing is selected just delete first letter in after sequence
    then (TextEditor q [] (tail a) b )
    else (TextEditor q [] a b)-- else delete selected, leaving a alone

-- backspace deletes before cursor
backspace :: TextEditor -> TextEditor
backspace (TextEditor q s a b) = 
    (TextEditor (reverse(tail (reverse q))) [] a b)-- reverse then tail and then reverse to delete last item of q

moveCursorCharacterLeft :: TextEditor -> TextEditor -- move cursor to left one
moveCursorCharacterLeft (TextEditor q s a b) =
    (TextEditor (reverse(tail(reverse q))) [] ([(head (reverse q))]++a) b)-- return q without last item and concatanate last item of q with a

moveCursorCharacterRight :: TextEditor -> TextEditor --move cursor to right by one
moveCursorCharacterRight (TextEditor q s a b) =
        (TextEditor (q++[head a]) [] (tail a) b) -- adds first item of after cursor to before and delete the first item after cursor

moveCursorWordLeft :: TextEditor -> TextEditor -- move cursor to next work to left
moveCursorWordLeft (TextEditor q s a b) = search (moveCursorCharacterLeft(TextEditor q s a b)) where
 search (TextEditor q s a b)
  |null q = (TextEditor q s a b)
  |(head(reverse q) == ' ' && head(s++a) /= ' ') = (TextEditor q s a b)
  |otherwise = (search (moveCursorCharacterLeft(TextEditor q s a b)))

moveCursorWordRight :: TextEditor -> TextEditor
moveCursorWordRight (TextEditor q s a b) = search (moveCursorCharacterRight(TextEditor q s a b)) where
 search (TextEditor q s a b)
  |null a = (TextEditor q s a b)
  |((head a) /= ' ' && (last q) == ' ') = (TextEditor q s a b)
  |otherwise = (search (moveCursorCharacterRight(TextEditor q s a b)))

moveCursorLineStart :: TextEditor -> TextEditor
moveCursorLineStart (TextEditor q s a b) = (TextEditor [] [] (q++s++a) b) -- sets cursor to start of line by setting everything after cursor

moveCursorLineEnd :: TextEditor -> TextEditor
moveCursorLineEnd(TextEditor q s a b) = (TextEditor (q++s++a) [] [] b) -- sets cursor to end of line by setting everything before cursor 

initiate :: TextEditor -> TextEditor
initiate (TextEditor q s a b) = 
    TextEditor "Line has been initiated!!!" [] [' '] [] -- initiates TextEditor ADT

insertCharacter :: TextEditor -> Char -> TextEditor
insertCharacter (TextEditor q s a b) characterSelected =
    (TextEditor (q++[characterSelected]) s a b) -- add inserted character to before the cursor

selectToLeft :: TextEditor -> TextEditor
selectToLeft (TextEditor [] s a b) = (TextEditor [] s a b) -- if left is emtpy then can't select left
selectToLeft (TextEditor q s a b) = (TextEditor (reverse(tail(reverse q))) ([head(reverse q)]++s) a b) -- else select left by taking charcter from before and concatanating to cursor.

selectToRight :: TextEditor -> TextEditor
selectToRight (TextEditor q s a b) = (TextEditor q (s++[head a]) (tail a) b)

selectToWordLeft :: TextEditor -> TextEditor
selectToWordLeft (TextEditor q s a b) = search (selectToLeft(TextEditor q s a b)) where
 search (TextEditor q s a b)
  |null q = (TextEditor q s a b) -- if left is emtpy then can't select left
  |(head(reverse q) == ' ' && (head (s++a)) /= ' ') = (TextEditor q s a b)
  |otherwise = (search (selectToLeft(TextEditor q s a b)))

-- selects whole word to right using recursion  
selectToWordRight :: TextEditor -> TextEditor
selectToWordRight (TextEditor q s [] b) = (TextEditor q s [] b)
selectToWordRight (TextEditor q s a b) = search (selectToRight(TextEditor q s a b)) where
 search (TextEditor q s a b)
  |null a = (TextEditor q s a b)
  |((head a) == ' ' && head(reverse(q)) /= ' ') = (TextEditor q s a b)
  |otherwise = (search (selectToRight(TextEditor q s a b)))

selectToLineStart :: TextEditor -> TextEditor 
selectToLineStart (TextEditor q s a b) = (TextEditor [] (q++s) a b)

selectToLineEnd :: TextEditor -> TextEditor 
selectToLineEnd (TextEditor q s a b) = (TextEditor q (s++a) [] b)

selectAll :: TextEditor -> TextEditor
selectAll (TextEditor q s a b) = (TextEditor [] (q++s++a) [] b)

copy :: TextEditor -> TextEditor
copy (TextEditor q s a b) = (TextEditor q s a s)

cut :: TextEditor -> TextEditor
cut (TextEditor q s a b) = (TextEditor q [] a s)

paste :: TextEditor -> TextEditor
paste (TextEditor q s a b) = (TextEditor (q++b) s a [])
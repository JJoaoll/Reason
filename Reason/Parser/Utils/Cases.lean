
/- Counts the numbers of lines before certain Pos -/
def countNewlinesUntil (s : String) (pos : String.Pos) : Nat :=
  let substr := s.extract ⟨0⟩ pos
  substr.foldl (fun count char => if char == '\n' then count + 1 else count) 0

#eval
  let txt :=
"
asdas

sdasd
Sdasdsa
o

ERROR

asdasiodjas

"
countNewlinesUntil txt (txt.posOf 'R')
  -- countNewlinesUntil

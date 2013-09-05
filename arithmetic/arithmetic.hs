data Term =
    TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term
  | TmZero Info
  | TmSucc Info Term
  | TmPred Info Term
  | TmIsZero Info Term

newtype Info = Info Span

newtype Span = Span (Begin, End)

newtype Begin = Begin Pos

newtype End = End Pos

newtype Pos = Pos (Line, Col)

newtype Line = Line Int

newtype Col = Col Int

isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t1) = isNumericVal t1
isNumericVal _ = False

isVal t = case t of
    (TmTrue _) -> True
    (TmFalse _) -> True
    t | isNumericVal t -> True
    _ -> False
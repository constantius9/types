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

dummyinfo =
    Info(
        Span(
            (Begin(
                Pos(
                    Line(
                        (-1) ),
                    Col(
                        (-1) )
                )
            ) ),
            (End  (
                Pos(
                    Line(
                        (-1) ),
                    Col(
                        (-1) )
                )
            ) )
        )
    )


isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t1) = isNumericVal t1
isNumericVal _ = False

isVal t = case t of
    (TmTrue _) -> True
    (TmFalse _) -> True
    t | isNumericVal t -> True
    _ -> False

eval1 t = case t of
    TmIf _ (TmTrue _) t2 _ -> t2
    TmIf _ (TmFalse _) _ t3 -> t3
    TmIf fi t1 t2 t3 ->
        let t1' = eval1 t1
        in TmIf fi t1' t2 t3
    TmSucc fi t1 ->
        let t1' = eval1 t1
        in TmSucc fi t1'
    TmPred _ (TmZero _) -> TmZero dummyinfo
    TmPred _ (TmSucc _ nv1) | isNumericVal nv1 -> nv1
    TmPred fi t1 ->
        let t1' = eval1 t1
        in TmPred fi t1'
    TmIsZero _ (TmZero _) -> TmTrue dummyinfo
    TmIsZero _ (TmSucc _ nv1) | isNumericVal nv1 -> TmFalse dummyinfo
    TmIsZero fi t1 ->
        let t1' = eval1 t1
        in TmIsZero fi t1'
    _ -> undefined

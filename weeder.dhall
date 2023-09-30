{ roots =
    [ -- all main function supposed to be roots
      "^.*\\.main$"
    , -- all spec function supposed to be roots
      "^.*\\.spec$"
    -- admin package roots
    , "^Site.Foundation.*$"
    , "^Site.*$"
    -- all routes are assumed to be non-dead roots
    , "^.+\\.(get|post).+R$"
    -- tests and benchs
    , "^(.*)Spec.*$"
    , "^[bB]enchmarks?.*$"
    ]
, type-class-roots = True }
(**
# F# Enterprise - IO

IO in `FSharp.Enterprise` is abstracted by the `IIO` interface that defines the following types

        type SearchPattern = string
        type FileFilter = (string -> bool)
        type Path = string

        type IIO =
            abstract member Write : Path * 'a -> unit
            abstract member Read : Path -> 'a option
            abstract member ReadAll : Path * FileFilter -> seq<'a>
            abstract member ReadAll : seq<SearchPattern> -> seq<'a>
            abstract member Delete : Path -> unit

*)


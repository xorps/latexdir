module DB

open Microsoft.Data.SqlClient
open FSharp.Control.Tasks.V2.ContextInsensitive
open Control.TaskResultBuilder
open System.Threading.Tasks

type DBError = DBError of exn

let private exec<'T> conn q param f = task {
    try 
        use conn = new SqlConnection(conn)
        use cmd = conn.CreateCommand()
        cmd.CommandText <- q
        for p in param do cmd.Parameters.Add(p) |> ignore
        let! _ = conn.OpenAsync()
        let! _ = cmd.PrepareAsync()
        let! reader = cmd.ExecuteReaderAsync()
        try
            let! a = reader.ReadAsync()
            let mutable hasRows = a
            let mutable res: List<'T> = List.empty
            while hasRows do
                res <- [f reader] |> List.append res
                let! a = reader.ReadAsync()
                hasRows <- a
            return Ok res
        finally
            reader.Close()
    with e -> return Error (DBError e)
}

module Param =
    let NDC name (value : string) = 
        try
            let p = new SqlParameter()
            p.ParameterName <- name
            p.Value <- value
            Ok p
        with e -> Error (DBError e)

type Row = {NDC: string; latex_free: bool}

let findOne (conn : string) (ndc : string): Task<Result<Row option, DBError>> = taskresult {
    let query = "SELECT ndc, latex_free FROM latex WHERE ndc = @NDC LIMIT 1";
    let! param = Task.FromResult (Param.NDC "@NDC" ndc)
    let! res = exec conn query [| param |] (fun r -> {NDC = r.GetString(0); latex_free = r.GetBoolean(1)})
    return res |> List.tryHead
}

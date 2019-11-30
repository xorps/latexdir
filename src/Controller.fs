namespace Controller

open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open Control
open Microsoft.Extensions.Configuration

[<ApiController>]
type AppController (logger : ILogger<AppController>) =
    inherit ControllerBase()

    member private self.html (s : string) = self.Content(s, "text/html", System.Text.Encoding.UTF8)

    [<HttpGet("")>]
    member self.Index() = self.html (View.render (View.index))

    [<HttpGet("search")>]
    member self.Search([<FromQuery>] query : string, [<FromServices>] conf : IConfiguration) = 
        let conn = conf.GetValue<string>("db_string")
        let log msg obj = logger.LogError(msg + ": " + obj.ToString(), [| obj |])
        query
        |> Search.APIv3.api conn log
        |> Task.map self.html
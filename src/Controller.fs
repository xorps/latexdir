namespace Controller

open Microsoft.AspNetCore.Mvc
open Microsoft.Extensions.Logging
open Control

[<ApiController>]
type AppController (logger : ILogger<AppController>) =
    inherit ControllerBase()

    member private self.html (s : string) = self.Content(s, "text/html", System.Text.Encoding.UTF8)

    [<HttpGet("")>]
    member self.Index() = self.html (View.render (View.index))

    [<HttpGet("search")>]
    member self.Search([<FromQuery>] query : string) = 
        query
        |> Search.APIv1.api
        |> Task.map self.html
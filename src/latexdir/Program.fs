module latexdir.App

open System
open System.IO
open System.Net.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.Http
open Control
open Control.TaskResult
open Digits
open Number.Types
open Number.Values
open Builder.Option
open FSharp.Control.Tasks.V2.ContextInsensitive
open Newtonsoft.Json
open System.Threading.Tasks

// ---------------------------------
// Models
// ---------------------------------

type Message =
    {
        Text : string
    }

// ---------------------------------
// Views
// ---------------------------------

module Views =
    open GiraffeViewEngine

    let layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "Latex Directory" ]
                meta [ _name "viewport"; _content "height=device-height, width=device-width, initial-scale=1" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let partial () =
        h1 [] [ img [ _src "/LatexDirectoryBeta.png" ] ]

    let index (model : Message) =
        [
            partial()
            form [ _action "/search"; _method "get" ] [
                p [] [ 
                    input [ _name "query"; _type "text" ] 
                ]
            ]
        ] |> layout

// ---------------------------------
// Web app
// ---------------------------------

let indexHandler (name : string) =
    let greetings = sprintf "Hello %s, from Giraffe!" name
    let model     = { Text = greetings }
    let view      = Views.index model
    htmlView view

module Search =
    type ServiceError =
        | HttpError of exn
        | UnhandledStatusCode of Net.HttpStatusCode

    type SearchError =
        | InvalidInput
        | NoResults
        | FailedJsonParse of exn
        | ServiceError of ServiceError
        | EmptyGetString of SearchError []
        | EmptyJson of SearchError []

    type Product = {BrandName: String; Mfg: String}

    type SearchResult<'T> = Result<'T, SearchError>
    type SearchTask<'T> = Task<SearchResult<'T>>

    let ok_or err = function
        | Some a -> Ok a
        | None -> Error err

    let getString (url : string): SearchTask<string> = task {
        use client = new HttpClient()
        try
            let! res = client.GetAsync url
            match res.StatusCode with
            | Net.HttpStatusCode.OK ->
                let! body = res.Content.ReadAsStringAsync()
                return Ok body
            | Net.HttpStatusCode.NotFound -> return Error NoResults
            | code -> return Error (ServiceError (UnhandledStatusCode code))
        with e -> return Error (ServiceError (HttpError e))
    }
    
    let url ndc = sprintf "https://api.fda.gov/drug/ndc.json?search=packaging.package_ndc:\"%s\"&limit=1" (Barcode.NDC_10.AsString ndc)

    module DTO =
        type Results = {skip: int; limit: int; total: int}
        type Meta = {disclaimer: String; terms: String; license: String; last_updated: String; results: Results}
        type Result = {generic_name: String; labeler_name: String; brand_name: String}
        type T = {meta: Meta; results: Result []}
    
    let parse (s : string): SearchResult<Product> = 
        try
            let dto = JsonConvert.DeserializeObject<DTO.T> s
            Ok ({
                BrandName = dto.results.[0].brand_name
                Mfg = dto.results.[0].labeler_name
            })
        with e -> Error (FailedJsonParse e)

    let permutations (ctx : HttpContext): SearchResult<Barcode.NDC_10 []> =
        let res = option {
            let! query = ctx.TryGetQueryStringValue("query")
            let! digits = Digits.create query _10
            let! ndcs = Barcode.possible digits
            return ndcs
        }
        res |> ok_or InvalidInput

    let join<'T, 'E, 'B> (err : 'E [] -> 'B) (r : Result<'T, 'E> []): Result<'T [], 'B> =
        let successes = r |> Array.choose (fun it -> match it with | Ok a -> Some a; | _ -> None)
        let errors = r |> Array.choose (fun it -> match it with | Error e -> Some e; | _ -> None)
        if Array.isEmpty successes then
            Error (err errors)
        else
            Ok successes
        
    let api (next : HttpFunc) (ctx : HttpContext) = task {
        let! res = taskresult {
            let! ndcs = Task.from (permutations ctx)
            let! ndcs = Task.WhenAll (ndcs |> Array.map url |> Array.map getString) |> Task.map (join EmptyGetString)
            let! ndcs = Task.from (ndcs |> Array.map parse |> join EmptyJson)
            return ndcs
        }
        match res with
        | Ok products -> return! ctx.WriteStringAsync (products |> Array.map Object.ToString |> String.concat "\n")
        | Error err -> 
            match err with
            | InvalidInput -> return! ctx.WriteStringAsync "Invalid Input"
            | NoResults -> return! ctx.WriteStringAsync "No Results"
            | EmptyGetString e -> return! ctx.WriteStringAsync ("No Results - Empty Get String: " + (e |> Array.map Object.ToString |> String.concat ","))
            | _ as e -> return! ctx.WriteStringAsync (e.ToString())
    }

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> indexHandler "world"
                route "/search" >=> Search.api
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true  -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseHttpsRedirection()
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddFilter(fun l -> l.Equals LogLevel.Error)
           .AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0
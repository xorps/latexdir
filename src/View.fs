module View

open Giraffe.GiraffeViewEngine

type Product = {BrandName: string; NDC: string; GenericName: string; Manufacturer: string}

let render = renderHtmlDocument

let layout (content: XmlNode list) =
    html [] [
        head [] [
            title []  [ encodedText "Latex Directory" ]
            meta [ _name "viewport"; _content "height=device-height, width=device-width, initial-scale=1" ]
            link [ _rel  "stylesheet"; _type "text/css"; _href "/main.css" ]
        ]
        body [] content
    ]

let logo =
    h1 [ _class "logo" ] [ img [ _class "logo"; _src "/LatexDirectoryBeta.png" ] ]

let searchForm = 
    form [ _action "/search"; _method "get" ] [
        p [ _class "search" ] [ 
            input [ _class "search"; _name "query"; _type "text" ] 
        ]
    ]

let index =
    [
        logo
        searchForm
    ] |> layout

let showText (text : string) =
    [
        logo
        searchForm
        p [ _class "search-result" ] [ encodedText text ]
    ] |> layout

let private product (prod : Product) = div [ _class "product" ] [
    div [ _class "img" ] [ img [ _src "/vial.png" ] ]
    div [ _class "text" ] [
        p [] [ encodedText prod.NDC ]
        p [] [ encodedText prod.BrandName ]
        p [] [ encodedText prod.GenericName ]
        p [] [ encodedText prod.Manufacturer ]
    ]
    div [ _class "clear" ] []
]

let products (prod : Product []) =
    let products = prod |> Array.map product |> Array.toList
    [
        logo
        searchForm
        div [] products
    ] |> layout
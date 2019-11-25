module Startup =
    open Microsoft.Extensions.Configuration
    open Microsoft.Extensions.DependencyInjection
    open Microsoft.Extensions.Hosting
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting

    type Startup private () =
        new (configuration: IConfiguration) as this =
            Startup() then
            this.Configuration <- configuration

        // This method gets called by the runtime. Use this method to add services to the container.
        member this.ConfigureServices(services: IServiceCollection) =
            // Add framework services.
            services.AddControllers() |> ignore

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        member this.Configure(app: IApplicationBuilder, env: IWebHostEnvironment) =
            if (env.IsDevelopment()) then
                app.UseDeveloperExceptionPage() |> ignore

            app.UseHttpsRedirection() |> ignore
            app.UseStaticFiles() |> ignore
            app.UseRouting() |> ignore

            app.UseAuthorization() |> ignore

            app.UseEndpoints(fun endpoints ->
                endpoints.MapControllers() |> ignore
                ) |> ignore

        member val Configuration : IConfiguration = null with get, set

module Program =
    open Startup
    open Microsoft.AspNetCore.Hosting
    open Microsoft.Extensions.Hosting

    let CreateHostBuilder args =
        Host.CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(fun webBuilder ->
                webBuilder.UseStartup<Startup>() |> ignore
            )

    [<EntryPoint>]
    let main args =
        CreateHostBuilder(args).Build().Run()
        0
FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["Fluff.App/Fluff.App.fsproj", "Fluff.App/"]
RUN dotnet restore "Fluff.App/Fluff.App.fsproj"
COPY . .
WORKDIR "/src/Fluff.App"
RUN dotnet build "Fluff.App.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Fluff.App.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Fluff.App.dll"]

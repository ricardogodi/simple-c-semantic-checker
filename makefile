# Makefile for simple-c-semantic-checker

PROJECT_FILE = FSharp-Nix.fsproj

build:
	dotnet build $(PROJECT_FILE)

run:
	dotnet run --project $(PROJECT_FILE)

clean:
	dotnet clean $(PROJECT_FILE)
	rm -rf bin obj
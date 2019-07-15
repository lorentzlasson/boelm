mkdir -p dist
cp src/web/* dist/
elm make src/Main.elm --output=dist/main.js

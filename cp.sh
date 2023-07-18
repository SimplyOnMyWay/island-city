#!/bin/sh

mkdir ~/cljs/gac/prod/01_beith/cljs-out
cp ./target/public/cljs-out/opt-main.js ~/cljs/gac/prod/01_beith/cljs-out/opt-main.js
echo "copied target/public/cljs-out/opt-main.js to prod/01_beith/cljs-out/opt-main.js"
cp -r ./target/public/webck/ ~/cljs/gac/prod/01_beith/webck/
echo "copied (recursively) target/public/webck/ to prod/01_beith/webck"
cp -r ./resources/public/* ~/cljs/gac/prod/01_beith/
echo "copied (recursively) resources/public/ to prod/01_beith/"
sed -i 's/dev-main.js/opt-main.js/g' ~/cljs/gac/prod/01_beith/index.html
echo "sed replaced dev-main.js with opt-main.js in index.html"






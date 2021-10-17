elm make src/Main.elm --output=main.js --optimize
.\node_modules\uglify-js\bin\uglifyjs main.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | .\node_modules\uglify-js\bin\uglifyjs --mangle --output main.min.js
copy index.html ..\farmer-rails\public
copy main.min.js ..\farmer-rails\public

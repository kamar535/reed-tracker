## Elm App

This project is bootstrapped with [Create Elm
App](https://github.com/halfzebra/create-elm-app). Read more in
[ElmApp.md](ElmApp.md).

## Start local web server. 

```
elm-app start
```

For some reason, I get the following error. 

```
Starting the development server...

Error: error:0308010C:digital envelope routines::unsupported
    at new Hash (node:internal/crypto/hash:71:19)
    at Object.createHash (node:crypto:133:10)
    at module.exports (/usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/util/createHash.js:135:53)
    at NormalModule._initBuildHash (/usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/NormalModule.js:417:16)
    at handleParseError (/usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/NormalModule.js:471:10)
    at /usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/NormalModule.js:503:5
    at /usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/NormalModule.js:358:12
    at /usr/local/lib/node_modules/create-elm-app/node_modules/loader-runner/lib/LoaderRunner.js:373:3
    at iterateNormalLoaders (/usr/local/lib/node_modules/create-elm-app/node_modules/loader-runner/lib/LoaderRunner.js:214:10)
    at iterateNormalLoaders (/usr/local/lib/node_modules/create-elm-app/node_modules/loader-runner/lib/LoaderRunner.js:221:10)
/usr/local/lib/node_modules/create-elm-app/scripts/start.js:11
  throw err;
  ^

Error: error:0308010C:digital envelope routines::unsupported
    at new Hash (node:internal/crypto/hash:71:19)
    at Object.createHash (node:crypto:133:10)
    at module.exports (/usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/util/createHash.js:135:53)
    at NormalModule._initBuildHash (/usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/NormalModule.js:417:16)
    at /usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/NormalModule.js:452:10
    at /usr/local/lib/node_modules/create-elm-app/node_modules/webpack/lib/NormalModule.js:323:13
    at /usr/local/lib/node_modules/create-elm-app/node_modules/loader-runner/lib/LoaderRunner.js:367:11
    at /usr/local/lib/node_modules/create-elm-app/node_modules/loader-runner/lib/LoaderRunner.js:233:18
    at context.callback (/usr/local/lib/node_modules/create-elm-app/node_modules/loader-runner/lib/LoaderRunner.js:111:13)
    at /usr/local/lib/node_modules/create-elm-app/node_modules/babel-loader/lib/index.js:59:103 {
  opensslErrorStack: [ 'error:03000086:digital envelope routines::initialization error' ],
  library: 'digital envelope routines',
  reason: 'unsupported',
  code: 'ERR_OSSL_EVP_UNSUPPORTED'
}

Node.js v18.12.1
```

This [workaround][workaround] solves the issue.

[workaround]:
    https://stackoverflow.com/questions/69692842/error-message-error0308010cdigital-envelope-routinesunsupported

```
export NODE_OPTIONS=--openssl-legacy-provider
elm-app start
```

Or, if you don't like the export:

```
NODE_OPTIONS=--openssl-legacy-provider elm-app start
```
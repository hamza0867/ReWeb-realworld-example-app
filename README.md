# real-world-app


[![CircleCI](https://circleci.com/gh/yourgithubhandle/real-world-app/tree/master.svg?style=svg)](https://circleci.com/gh/yourgithubhandle/real-world-app/tree/master)


**Contains the following libraries and executables:**

```
real-world-app@0.0.0
│
├─test/
│   name:    TestRealWorldApp.exe
│   main:    TestRealWorldApp
│   require: real-world-app.lib
│
├─library/
│   library name: real-world-app.lib
│   namespace:    RealWorldApp
│   require:
│
└─executable/
    name:    RealWorldAppApp.exe
    main:    RealWorldAppApp
    require: real-world-app.lib
```

## Developing:

```
npm install -g esy
git clone <this-repo>
esy install
esy build
```

## Running Binary:

After building the project, you can run the main binary that is produced.

```
esy x RealWorldAppApp.exe 
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```

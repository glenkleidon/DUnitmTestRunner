# DUnitmTestRunner
The DUnitm Test Runner is a Delphi console project that allows you automatically build and run all of the [DUnitm](https://github.com/glenkleidon/DelphiTips/wiki/DUnitm---Mini-Test-Framework) Test projects in a specified folder. This is similar to popular Javascript test runners like [Mocha](https://mochajs.org), [Karma](https://karma-runner.github.io/latest/index.html) and [Jest](https://jestjs.io/)

It is ideal to use with continuous delivery systems like Microsoft DevOps (TFS), Bamboo, Team City, Final Builder etc, and it is especially effective when using Delphi Project bundles.  

Like _*DUnitm*_ **all versions of Delphi are supported back to Delphi 5** (Delphi4 probably also works).  You can run tests for different versions of Delphi simply by specifying the version of Delphi in the command line if that version of delphi is installed AND your project has compatible language features. 

There is a single executable _*TestBuilder.exe*_ which can be installed locally or globally to run the tests.  

# Building TestRunner 

## Getting the Binaries


## How to Use the Test Runner
_*TestBuilder*_ is a command line console application which is controlled by command line options.  It creates a re-usable Windows **Batch Script** which it runs displaying the results in a viewer of your choice.

While the primary purpose of the TestRunner is to allow your test cases to be run within continuous Integration process, it can also run it manually at any time.

### Help Prompt
To see the command line options for _*TestBulder*_ use the /? or /help option
This will display the following.

```
C:\Testing>TestBuilder.exe /?

>TestBuilder [-|/][?|help] | [[-|/]b] [[-|/][EXCLUDE[:]<[*]|Project;Project...>

     [[-|/][INCLUDE[:]<Project1;Project2...>] [[-|/]S:<ScriptName>]
     [[-|/]V:<DelphiVersion>] [[-|/]O:<OpenWith>]
     <StartInFolder>

 ?|help           : This Message
 b                : Build SCRIPT ONLY, do not RUN
 EXCLUDE          : List of Project Names to exclude ("*"=All)
                    eg: /EXCLUDE:TestMyFunc;TestMyProc
 INCLUDE          : List of Project Names to include (if otherwise excluded)
                    eg: /INCLUDE:TestMyFunc;TestMyProc
 V:<DelphiVersion>: The version of delphi to build. Use short name or VersionId
                    eg: -v:D7 -v:Delphi7 -v:XE2 -v:Berlin -v:Seattle etc
 O:<OpenWith>     : Open the test result with a specific program default (none)
                    eg: -O:Notepad - opens the Test results in notepad
 <ScriptName>     : Full pathname to Test Runner Script
                  (<StartInFolder>TestRunner.bat if not supplied)
 <StartInFolder>  : (Required) Folder to recursively search for test projects.

C:\Users\glen.GALKAMLAN\Documents\DUnitmTestRunner\win32\debug>
```

## Examples

### Run all tests in a folder 



## Project Roadmap
 + NUnit XML Report format.  This will extend the "Open with" functionality 
 + Delphi IDE Integration: Test WorkBench.  Allows you to Select and run individual tests - in a similar way to Visual Studio.











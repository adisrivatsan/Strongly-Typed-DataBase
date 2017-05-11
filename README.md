# Fresh

Fresh is a Haskell micro-library enabling Haskell artists to take their recursive (and non-recursive) data structures and share them with remote clients in a real-time, interactive manner through a common JSON interface. 

## Setup

Be sure to install the required dependencies listed in the `.cabal` file.

Run `ghci Main.hs` and `main` in the ghci prompt, then connect to `ws://localhost:8080` using a websocket connection software of your choice. 

Run the demo client by running `ghci Frontend/FrontendMain.hs`

## Validation

The real power of Fresh is that it takes advantage of Haskell's rich typing system to enforce strong constraints on a typically unconstrained JSON. By default, Fresh uses our `RecursiveValidator` class to validate inputs. 

While `RecursiveValidator` provides the most flexible validation system as it permits the developer to create a hierarchy of `(Value) -> Bool` validation functions, it does not capture some of the power of Haskell's logical constructors. We have developed another use-case `FamilyTree`, to demonstrate that our solution generalizes to any validatable data structure (that is, any data structure which conforms to our `Validatable` typeclass).

Developers can also create their own validation structures:

1. Create your validated type in the `Plugins/` directory, ensure it conforms to the `Validatable` typeclass
2. Import it in `Configurable/ServerStoreFormat.hs` and set `ServerStoreFromat` equal to it

That's it!

## Directory Structure

`Main.hs`          - Entry point for App

`Server/Core.hs`   - Handles all core networking logic

`Server/ClientHandle.hs` - Contains Client Handle Logic used by Server

`Common.hs`        - Contains common definitions needed

`Tests.hs`         - QuickCheck properties and HUnit testes

`Utils/AesonAdditions.hs` - Minor additions to Aeson framework which should honestly already be in it

`Plugins/RecursiveValidator.hs` - The `RecursiveValidator` validated storage structure

`Plugins/FamilyTree.hs` - The `FamilyTree` validated storage structure

`Configurable/ServerStoreFormat.hs` - The type structure the server will use for storage and validation - **configurable**

`Frontend/FrontendMain.hs` - Entry point for client demo

`Frontend/`

## Client-Side Usage

### SUBSCRIBE

Subscribe to a location in the data store by sending a JSON message of the form

    {
        "path": "son/daughter/daughter/age"
    }
    
Whenever another client directly modifies the data at this location, the server will send a message to you of the form

    son/daughter/daughter/age: 10

### PUT

Modify the contents at a location in the data store by sending a JSON message of the form

    {
        "path": "son/daughter/daughter",
        "value": { 
                     "age": 10
                 }
    }
    
Keep in mind this operation will overwrite whatever contents were previously stored at this location (i.e. this daughter will become a leaf node).

## Why Fresh?

Fresh is built on two ideological pillars: keeping data updated, and keeping systems stable. The industry is moving more and more towards untyped realtime-enabled systems such as CouchDB, MongoDB, and Firebase. While this enables organizations to iterate faster, it reduces system stability and exposes users to nasty runtime bugs. Fresh aims to fix this by allowing developers to specify validation patterns on the server which are shared with the client, ensuring a seamless, strongly-typed networking transfer from start to end. 
# Design

![design](https://raw.githubusercontent.com/mhkoji/Hachee/master/senn/doc/design.svg)
```uml
@startuml
package "senn" {
 [ime]
}

package "fcitx-senn" {
  folder "frontend" {
    [DoInput]
    [ui]
  }
             
  folder "backend" {
    [menu]
    folder "stateful-im" {
      [handle-request]
    }

    folder "input-processor" {
      [process-input]
      [state]
    }
  }
  
  [DoInput] -- [handle-request] : process-input
  [handle-request] -- [process-input]
  [process-input] -- [state]
  [process-input] - [ime] : convert, lookup, predict

  [ui] - [menu] : spawn
}


:User: -- DoInput : key
:User: -- ui: click
@enduml
```

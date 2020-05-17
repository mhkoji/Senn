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
    [stateful-im]
    folder "input-processor" {
      [state]
    }
  }
  
  [ui] - [menu] : spawn
  [DoInput] -- [stateful-im] : process-input
  [stateful-im] - [state]
  [stateful-im] -- [ime] : convert, lookup, predict
}
  
:User: -- DoInput : key
:User: -- ui: click
                                                
@enduml
```

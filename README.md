A playground for exploring critical chain project management
via a Scala DSL.

You can add tasks to a plan, and state dependencies between them.
Tasks can have a duration and a resource.

```scala
  new Plan {
    add task 't0  as "Assemble the team"
    add task 't1a as "Brief the team" duration 4
    add task 't1b as "Be awesome"
    add task 'end
    't0 ~> 't1a ~> 't1b
    't1b ~> 'end
  }
```

Here is some other syntax:

```
  // Resources must be declared before use
  declare resource "Alice"
  
  // Define a task with just an id
  add task 'start
  
  // Define a task with a description (id is auto-generated)
  add task "Implement login"
  
  // Define a task with a resource
  add task 'rw3 as "Rewire 3rd floor" resource "Alice"
  
  // Everything
  add task 'door as "Install automatic door" duration 2 resource "Bob"
```

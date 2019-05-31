How would a program using a module break if we changed the type defined in the module, but it wouldn't if we hid the type from the user?

I think it's because if we hide the type definition from the user, the user isn't able to work directly with it. And can only use it in terms of the functions declared in the module. And hence if the type were to change, we'd update our functions accordingly. 
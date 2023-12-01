let first str ~r = Re.Group.get (Re.exec r str) 0

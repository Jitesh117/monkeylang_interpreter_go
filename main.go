package main

import (
	"fmt"
	"os"
	"os/user"

	"monkey/repl"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s! Let's take this REPL on a ride. Hop on!\n",
		user.Username)
	repl.Start(os.Stdin, os.Stdout)
}

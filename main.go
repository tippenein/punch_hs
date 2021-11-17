package main

import (
	"database/sql"
	// "encoding/json"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"log"
	"os"
	"time"
)

// var only_table = `
// 	create table tasks(
// 		id integer primary key,
// 		task text,
// 		intime utctime not null,
// 		outtime utctime);
// `

// 'punch in project_name'
// punch out
// punch list project_name
func main() {
	path := "/home/brady/punch.db"
	db, err := sql.Open("sqlite3", path)
	if err != nil {
		log.Fatal(err)
	}
	punched_in, err := db.Query("select * from tasks where outtime is null order by intime desc limit 1")
	defer punched_in.Close()

	args := os.Args[1:]
	command := args[0]
	switch {
	case command == "in":
		task := args[1]
		any := false
		for punched_in.Next() {
			any = true
		}
		if any {
			fmt.Println("can't punch in again")
		} else {
			// add new intime entry
			stmt, err := db.Prepare("insert into tasks(task, intime, outtime) values(?, datetime(), null)")
			defer stmt.Close()

			if err != nil {
				log.Fatal(err)
			}
			_, err = stmt.Exec(task)
			if err != nil {
				log.Fatal(err)
			}
			fmt.Printf("punched into %s", task)
		}

	// case command == "new":
	// 	stmt, err := db.Prepare(only_table)
	// 	defer stmt.Close()

	// 	if err != nil {
	// 		log.Fatal(err)
	// 	}
	// 	_, err = stmt.Exec()
	// 	if err != nil {
	// 		log.Fatal(err)
	// 	}
	// 	fmt.Printf("initialized punch.db", task)

	case command == "out":
		var id int
		var task string
		for punched_in.Next() {
			var i time.Time
			var o time.Time
			err = punched_in.Scan(&id, &task, &i, &o)
		}

		stmt, err := db.Prepare("update tasks set outtime = datetime() where id = ?")

		defer stmt.Close()

		if err != nil {
			log.Fatal(err)
		}
		_, err = stmt.Exec(id)

		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("punched out from %s", task)

	case command == "list":
		fmt.Println("not implemented yet")
	}

}

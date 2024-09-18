# Comparing a packet to itself returns an empty diff

    Code
      print(result)
    Output
      v Packets 19700101-000000-00000001 and 19700101-000000-00000001 are identical

# Comparing packets ignores ID and time differences

    Code
      print(result)
    Output
      v Packets 19700101-000000-00000001 and 19700101-000000-00000002 are identical

# Can compare packets with different metadata

    Code
      print(everything)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! Packet metadata differs:
        < 19700101-000000-00000001
        > 19700101-000000-00000002
        @@ 7,5 / 7,5 @@
          $parameters
          $parameters$A
        < [1] "foo"
        > [1] "bar"
          
          

---

    Code
      print(metadata)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! Packet metadata differs:
        < 19700101-000000-00000001
        > 19700101-000000-00000002
        @@ 7,5 / 7,5 @@
          $parameters
          $parameters$A
        < [1] "foo"
        > [1] "bar"
          
          

---

    Code
      print(files)
    Output
      v Files of packets 19700101-000000-00000001 and 19700101-000000-00000002 are identical

# Can compare packets with different file contents

    Code
      print(everything)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following files exist in both packets but have different contents:
      * data.R
      * data.txt
      i Print the comparison with `verbose = TRUE` to display the differences in the files' contents

---

    Code
      print(metadata)
    Output
      v Metadata of packets 19700101-000000-00000001 and 19700101-000000-00000002 is identical

---

    Code
      print(files)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following files exist in both packets but have different contents:
      * data.R
      * data.txt
      i Print the comparison with `verbose = TRUE` to display the differences in the files' contents

---

    Code
      print(files, verbose = TRUE)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following files exist in both packets but have different contents:
      * data.R
        < 19700101-000000-00000001/data.R
        > 19700101-000000-00000002/data.R
        @@ 1 / 1 @@
        < writeLines("Hello", "data.txt")
        > writeLines("World", "data.txt")
      * data.txt
        < 19700101-000000-00000001/data.txt
        > 19700101-000000-00000002/data.txt
        @@ 1 / 1 @@
        < Hello
        > World

# Can compare artefacts only

    Code
      print(files)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following files exist in both packets but have different contents:
      * data.R
      i Print the comparison with `verbose = TRUE` to display the differences in the files' contents

# Can detect newly declared artefact

    Code
      print(artefacts)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following artefacts only exist in packet 19700101-000000-00000001:
      * hello.txt

# Can compare packets with binary contents

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following artefacts exist in both packets but have different contents:
      * data.rds
      i Print the comparison with `verbose = TRUE` to display the differences in the artefacts' contents

---

    Code
      print(result, verbose = TRUE)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following artefacts exist in both packets but have different contents:
      * data.rds
      ! Contents of binary file data.rds were omitted


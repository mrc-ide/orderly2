# Comparing a packet to itself returns TRUE

    Code
      print(result)
    Output
      v Packets are identical

# Comparing packets ignores ID and time differences

    Code
      print(result)
    Output
      v Packets are identical

# Can compare packets with different metadata

    Code
      print(all)
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
          
          

# Can compare packets with different file contents

    Code
      print(all)
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
      < 19700101-000000-00000001/data.R
      > 19700101-000000-00000002/data.R
      @@ 1,4 / 1,4 @@
        {
            orderly_artefact("Output", "output.txt")
      <     writeLines(toString(2 + 1), "output.txt")
      >     writeLines(toString(1 + 2), "output.txt")
        }

# Can compare packets with binary contents

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002
      ! The following files exist in both packets but have different contents:
      * data.rds
      * data.txt
      < 19700101-000000-00000001/data.txt
      > 19700101-000000-00000002/data.txt
      @@ 1 / 1 @@
      < Hello
      > World


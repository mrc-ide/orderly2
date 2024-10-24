# Comparing a packet to itself returns an empty diff

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000001...
      v The two packets are identical.

# Comparing packets ignores ID and time differences

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      v The two packets are equivalent, up to trivial differences.

# Can explicitly compare trivial fields

    Code
      orderly_comparison_explain(result, "id")
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i Comparing attribute `id`
      < 19700101-000000-00000001$id   
      > 19700101-000000-00000002$id   
      @@ 1 / 1 @@                     
      < [1] "19700101-000000-00000001"
      > [1] "19700101-000000-00000002"

# Can compare packets with different metadata

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i The following attributes are different across the two packets:
      * parameters
      i Use `orderly_comparison_explain(...)` to examine the differences in more detail.

---

    Code
      orderly_comparison_explain(result)
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i Comparing attribute `parameters`
      < 19700101-000000-00000001$parameters
      > 19700101-000000-00000002$parameters
      @@ 1,3 / 1,3 @@  
        $A             
      < [1] "foo"      
      > [1] "bar"      
                       

---

    Code
      orderly_comparison_explain(result, "files")
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      v The specified attributes are identical across the two packets.

# Can compare packets with different file contents

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i The following attributes are different across the two packets:
      * files
      i Use `orderly_comparison_explain(...)` to examine the differences in more detail.

---

    Code
      orderly_comparison_explain(result, "files")
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i The following files exist in both packets but have different contents:
        * data.R
        * data.txt
      i Use `orderly_comparison_explain(..., "files", verbose = TRUE)` to compare the files' contents.

---

    Code
      orderly_comparison_explain(result, "files", verbose = TRUE)
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      < 19700101-000000-00000001/data.R
      > 19700101-000000-00000002/data.R
      @@ 1 / 1 @@                      
      < writeLines("Hello", "data.txt")
      > writeLines("World", "data.txt")
      < 19700101-000000-00000001/data.txt
      > 19700101-000000-00000002/data.txt
      @@ 1 / 1 @@  
      < Hello      
      > World      

# Can compare packets with binary contents

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i The following attributes are different across the two packets:
      * files
      i Use `orderly_comparison_explain(...)` to examine the differences in more detail.

---

    Code
      orderly_comparison_explain(result, "files")
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i The following files exist in both packets but have different contents:
        * data.R
        * data.rds
      i Use `orderly_comparison_explain(..., "files", verbose = TRUE)` to compare the files' contents.

---

    Code
      orderly_comparison_explain(result, "files", verbose = TRUE)
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      < 19700101-000000-00000001/data.R                         
      > 19700101-000000-00000002/data.R                         
      @@ 1,4 / 1,4 @@                                           
        {                                                       
            orderly_artefact(description = "Output", "data.rds")
      <     saveRDS(1:10, "data.rds")                           
      >     saveRDS(11:20, "data.rds")                          
        }                                                       
      ! The following files differ across packets, but could not be compared as their content is binary:
        * data.rds

# Can compare packets from remote

    Code
      orderly_comparison_explain(result, "files")
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i The following files exist in both packets but have different contents:
        * data.rds
      i Use `orderly_comparison_explain(..., "files", verbose = TRUE)` to compare the files' contents.

---

    Code
      orderly_comparison_explain(result, "files", verbose = TRUE)
    Message
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      ! The following files differ across packets, but could not be compared as their content is binary:
        * data.rds

# Handles new attributes gracefully

    Code
      print(result)
    Output
      i Comparing packets 19700101-000000-00000001 and 19700101-000000-00000002...
      i The following attributes only exist in packet 19700101-000000-00000001:
      * new_key
      i The following attributes are different across the two packets:
      * files
      i Use `orderly_comparison_explain(...)` to examine the differences in more detail.

---

    Code
      print(result_swap)
    Output
      i Comparing packets 19700101-000000-00000002 and 19700101-000000-00000001...
      i The following attributes only exist in packet 19700101-000000-00000001:
      * new_key
      i The following attributes are different across the two packets:
      * files
      i Use `orderly_comparison_explain(...)` to examine the differences in more detail.


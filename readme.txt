Google drive link for html pages: https://drive.google.com/drive/u/1/folders/0B0vZY4L6De7pc1kzN3FsQWZ1a0U

Steps for running the code:-
1. Clear the memory from the console.
2. Load the libraries.
3. Compile all the functions.
4. Call the functions for all URLs and create a data frame and write HTML pages.
5. Write the data frame into file.


Function of the RScript:-
1. Main function of the script is to crawl and collect the data from Genime Biology Website.
2. The script has a separate function for each type of field for all journals.
3. We take individual urls for each journal and store them in a vector from the base page.
4. we navigate through the webpage for each field and extract the nodes for the data we want to collect.
5. In the functions, we have used regular expressions to find the type of data we need and then stored into their respective vectors.
6. While extracting the data, we also save the html pages as we navigate through the website.
7. We then combine the individual data vectors into a data frame.  


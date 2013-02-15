## 1.2-4 (February 15, 2013)

Major update to add new functionality

- **Cutoff Function:**
  - Added a new function which will permute random datasets and calculate the inverse rank product to estimate the significance of the results you observe using `ena`

- **Network Simulation Function:**
  - Using `simulateNetwork`, you can now simulate gene expression datasets of whatever size you desire by providing the underlying network topology. This function will allow you to test the performance of various network reconstruction metods.

- **New Helper Functions:**
  - Added `mat2adj` to convert from an adjacency matrix to an adjacency list.
  - Added `adj2mat` to convert from an adjacency list to an adjacency matrix.
  - Added `tri2mat` to convert the data extracted using the `upper.tri` function back to the complete matrix.
  

- **Extended Documentation:**
  - Updated function documentation for some parameters.
  - All functions now include examples.

- **Sample Network Data:**
  - We offer a `PPI` dataset which provides a handful of network topologies based on real Protein-Protein Interaction networks.
  - Added 6 networks ranging fom 17 genes to 1,345 genes in size.



## 1.1-103 (July 18, 2012)

Initial Release

- **Reconstruct GRNs**
  - Reconstruct GRNs using the **parmigene** pacakge (`buildAracne`)
  - Reconstruct GRNs using the **GeneNet** pacakge (`buildGenenet`)
  - Reconstruct GRNs using the **WGCNA** pacakge (`buildWgcna`)
  - Reconstruct GRNs using the **space** pacakge (`buildSpace`)
  
- **Inverse Rank Product**
	- Aggregate networks using the inverse rank product in the `ena` function.
	
- **Make a matrix symmetric**
	- The `symmetricize` function offers a few different ways to transform a non-symmetric matrix into a symmatric one.
#!/bin/bash
#SBATCH --job-name=test_memory
#SBATCH --output=test_memory.out
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:10:00
#SBATCH --partition=earth-1

# Print the allocated memory
echo "Allocated memory:"
cat /proc/meminfo | grep MemTotal

# Run a simple command
sleep 60
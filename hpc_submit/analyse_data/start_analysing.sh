#!/bin/bash
#SBATCH --job-name=get_data_jk
#SBATCH --mail-type=all
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=128G
#SBATCH --time=12:00:00
#SBATCH --partition=earth-1

module load USS/2022

module load gcc/9.4.0-pe5.34
module load miniconda3/4.12.0
module load lsfm-init-miniconda/1.0.0

conda_env_name="r"

# ## activate env
# shellcheck disable=SC2063
if [[ $(conda info --envs | grep -c '*') != 0 ]]; then
    echo "- Deactivate current environment and activate ${conda_env_name:?}"
    conda deactivate
    conda activate "${conda_env_name:?}"
else
    echo "- Activate ${conda_env_name:?}"
    conda activate "${conda_env_name:?}"
fi

echo "Start Script"
echo "#############################################"

Rscript /cfs/earth/scratch/kraftjul/DaAn_Projektarbeit/code/analyse.R

echo "#############################################"
echo "Complete Script"

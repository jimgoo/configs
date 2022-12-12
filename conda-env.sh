set -e

mamba install -c conda-forge \
      ipdb \
      seaborn \
      opencv \
      pillow \
      ipykernel \
      h5py \
      papermill

# mamba install pytorch torchvision torchaudio cudatoolkit=11.1 -c pytorch

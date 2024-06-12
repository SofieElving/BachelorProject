
from gwf import Workflow, AnonymousTarget


def create_feature_importance_df():
    inputs = []
    outputs = []
    options = dict(cores = '5', memory = '20g', walltime = '30:00:00', account = 'ctdna_nn_F2024')
    spec = f"Rscript evaluate_models.R"
    return AnonymousTarget(inputs = inputs, outputs = outputs, options = options, spec = spec)
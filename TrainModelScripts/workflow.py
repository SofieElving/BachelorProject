

import os
import sys
import json
import glob

from templates import create_feature_importance_df

from gwf import Workflow, AnonymousTarget

gwf = Workflow()

gwf.target_from_template(
    name = 'model_eval',
    template = create_feature_importance_df()
)

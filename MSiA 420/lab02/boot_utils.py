import numpy as np
import pandas as pd
from scipy.stats import norm
from typing import Dict,Callable,Union,List

def boot(
        data:Union[np.ndarray,pd.DataFrame],
        statistic:Callable,
        r:int,
        **kwargs) -> Dict:
    '''
    Generate `r` bootstrap replicates of a statistic applied to data.
    
    Arguments
    ---------
    data: np.ndarray, pd.DataFrame
        Data on which the statistic is to be computed
    
    statistic: callable
        A function which returns a statistic to be compute on data.

            ``statistic(data,idx,**kwargs) -> np.ndarray ``
        
        where ``idx`` is a 1-D array containing indices and ``**kwargs``
        are additional keyword rguments to completely specify the
        function.
    
    r: int
        The number of bootstrap runs

    Returns
    ---------
    A dictionary with the following keys:
        t0: array
            statistic applied to the original data
        t: array
            A 2D array with `r` rows each of which is a bootstrap replicate
            of the result of calling `statistic`
    '''
    
    # create dictionary object
    res_boot = {}
    
    # compute the statistic applied to original data
    res_boot['t0'] = statistic(data,np.arange(data.shape[0]),**kwargs)
    
    # run bootstrap loop
    t_list = [None]*r
    for i in range(r):
        # draw bootstrap sample and compute statistic
        idxs = np.random.choice(data.shape[0],size=data.shape[0],replace=True)
        t_list[i] = statistic(data,idxs,**kwargs)
    
    # combine results into array
    res_boot['t'] = np.row_stack(t_list)
    
    # return final argument
    return res_boot

def boot_ci(
    res_boot:Dict,
    col_idx:int=0,
    kind:str='basic',
    conf_list:List[float]=[0.95],
    ) -> pd.DataFrame:
    '''
    Returns bootstrap confidence intervals corresponding to each
    confidence level conf in `conf_list`

    Arguments
    ---------
    res_boot: dict
        Dictonary output from `boot`
    
    col_idx: int
        Index of the parameter for which the CI is to be computed.
        Valid only when the statistic is vector-valued
    
    kind: str (Default: 'basic')
        Type of confidence interval. Should be one of "basic" 
        (reflected CI) or "norm" (crude CI)
    
    conf_list: list
        List containing the confidence level(s) of the required 
        interval(s)
    
    Returns
    ---------
    ci : pd.DataFrame
        A DataFrame with columns "lower" and "upper", and `conf_list`
        as the index.
    '''

    # extract parameters
    if not isinstance(res_boot['t0'],np.ndarray) or res_boot['t0'].size == 1:
        t0 = res_boot['t0']
        t = res_boot['t'].flatten()
    else:
        t0 = res_boot['t0'][col_idx]
        t = res_boot['t'][:,col_idx]
    
    # generate CIs
    alpha = 1-np.array(conf_list)

    if kind == 'basic':
        lower = 2*t0 - np.quantile(t,1-alpha/2)
        upper = 2*t0 - np.quantile(t,alpha/2)
    elif kind == 'norm':
        z_conf = norm.ppf(1-alpha/2)
        t_std = t.std()
        lower = t0 - z_conf*t_std
        upper = t0 + z_conf*t_std
    else:
        raise ValueError('Invalid value for argument `kind`')

    # format results and return
    ci = pd.DataFrame(
        {'lower':lower,'upper':upper},
        index=pd.Index(conf_list,name='conf')
    )
    
    return ci
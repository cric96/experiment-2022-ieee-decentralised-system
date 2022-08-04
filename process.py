import numpy as np
import pandas as pd
import xarray as xr
import re
from pathlib import Path
import collections
import json
from celluloid import Camera
import geopandas
import osmnx as ox
from osmnx.projection import project_geometry
from mpl_toolkits.basemap import Basemap
import pickle
def distance(val, ref):
    return abs(ref - val)
vectDistance = np.vectorize(distance)

def cmap_xmap(function, cmap):
    """ Applies function, on the indices of colormap cmap. Beware, function
    should map the [0, 1] segment to itself, or you are in for surprises.

    See also cmap_xmap.
    """
    cdict = cmap._segmentdata
    function_to_map = lambda x : (function(x[0]), x[1], x[2])
    for key in ('red','green','blue'):
        cdict[key] = map(function_to_map, cdict[key])
#        cdict[key].sort()
#        assert (cdict[key][0]<0 or cdict[key][-1]>1), "Resulting indices extend out of the [0, 1] segment."
    return matplotlib.colors.LinearSegmentedColormap('colormap',cdict,1024)

def getClosest(sortedMatrix, column, val):
    while len(sortedMatrix) > 3:
        half = int(len(sortedMatrix) / 2)
        sortedMatrix = sortedMatrix[-half - 1:] if sortedMatrix[half, column] < val else sortedMatrix[: half + 1]
    if len(sortedMatrix) == 1:
        result = sortedMatrix[0].copy()
        result[column] = val
        return result
    else:
        safecopy = sortedMatrix.copy()
        safecopy[:, column] = vectDistance(safecopy[:, column], val)
        minidx = np.argmin(safecopy[:, column])
        safecopy = safecopy[minidx, :].A1
        safecopy[column] = val
        return safecopy

def convert(column, samples, matrix):
    return np.matrix([getClosest(matrix, column, t) for t in samples])

def valueOrEmptySet(k, d):
    return (d[k] if isinstance(d[k], set) else {d[k]}) if k in d else set()

def mergeDicts(d1, d2):
    """
    Creates a new dictionary whose keys are the union of the keys of two
    dictionaries, and whose values are the union of values.

    Parameters
    ----------
    d1: dict
        dictionary whose values are sets
    d2: dict
        dictionary whose values are sets

    Returns
    -------
    dict
        A dict whose keys are the union of the keys of two dictionaries,
    and whose values are the union of values

    """
    res = {}
    for k in d1.keys() | d2.keys():
        res[k] = valueOrEmptySet(k, d1) | valueOrEmptySet(k, d2)
    return res

def extractCoordinates(filename):
    """
    Scans the header of an Alchemist file in search of the variables.

    Parameters
    ----------
    filename : str
        path to the target file
    mergewith : dict
        a dictionary whose dimensions will be merged with the returned one

    Returns
    -------
    dict
        A dictionary whose keys are strings (coordinate name) and values are
        lists (set of variable values)

    """
    with open(filename, 'r') as file:
#        regex = re.compile(' (?P<varName>[a-zA-Z._-]+) = (?P<varValue>[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?),?')
        regex = r"(?P<varName>[a-zA-Z._-]+) = (?P<varValue>[^,]*),?"
        dataBegin = r"\d"
        is_float = r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?"
        for line in file:
            match = re.findall(regex, line.replace('Infinity', '1e30000'))
            if match:
                return {
                    var : float(value) if re.match(is_float, value)
                        else bool(re.match(r".*?true.*?", value.lower())) if re.match(r".*?(true|false).*?", value.lower())
                        else value
                    for var, value in match
                }
            elif re.match(dataBegin, line[0]):
                return {}

def extractVariableNames(filename):
    """
    Gets the variable names from the Alchemist data files header.

    Parameters
    ----------
    filename : str
        path to the target file

    Returns
    -------
    list of list
        A matrix with the values of the csv file

    """
    with open(filename, 'r') as file:
        dataBegin = re.compile('\d')
        lastHeaderLine = ''
        for line in file:
            if dataBegin.match(line[0]):
                break
            else:
                lastHeaderLine = line
        if lastHeaderLine:
            regex = re.compile(' (?P<varName>\S+)')
            return regex.findall(lastHeaderLine)
        return []

def openCsv(path):
    """
    Converts an Alchemist export file into a list of lists representing the matrix of values.

    Parameters
    ----------
    path : str
        path to the target file

    Returns
    -------
    list of list
        A matrix with the values of the csv file

    """
    regex = re.compile('\d')
    with open(path, 'r') as file:
        lines = filter(lambda x: regex.match(x[0]), file.readlines())
        return [[float(x) for x in line.split()] for line in lines]

def beautifyValue(v):
    """
    Converts an object to a better version for printing, in particular:
        - if the object converts to float, then its float value is used
        - if the object can be rounded to int, then the int value is preferred

    Parameters
    ----------
    v : object
        the object to try to beautify

    Returns
    -------
    object or float or int
        the beautified value
    """
    try:
        v = float(v)
        if v.is_integer():
            return int(v)
        return v
    except:
        return v

if __name__ == '__main__':
    # CONFIGURE SCRIPT
    # Where to find Alchemist data files
    directory = 'data'
    # Where to save charts
    output_directory = 'charts'
    # How to name the summary of the processed data
    pickleOutput = 'data_summary'
    # Experiment prefixes: one per experiment (root of the file name)
    experiments = ['simulation']
    floatPrecision = '{: 0.3f}'
    # Number of time samples 
    timeSamples = 100
    # time management
    minTime = 0
    maxTime = 36000#46000
    timeColumnName = 'time'
    logarithmicTime = False
    # One or more variables are considered random and "flattened"
    seedVars = ['seed', 'longseed']
    # Label mapping
    class Measure:
        def __init__(self, description, unit = None):
            self.__description = description
            self.__unit = unit
        def description(self):
            return self.__description
        def unit(self):
            return '' if self.__unit is None else f'({self.__unit})'
        def derivative(self, new_description = None, new_unit = None):
            def cleanMathMode(s):
                return s[1:-1] if s[0] == '$' and s[-1] == '$' else s
            def deriveString(s):
                return r'$d ' + cleanMathMode(s) + r'/{dt}$'
            def deriveUnit(s):
                return f'${cleanMathMode(s)}' + '/{s}$' if s else None
            result = Measure(
                new_description if new_description else deriveString(self.__description),
                new_unit if new_unit else deriveUnit(self.__unit),
            )
            return result
        def __str__(self):
            return f'{self.description()} {self.unit()}'
    
    centrality_label = 'H_a(x)'
    def expected(x):
        return r'\mathbf{E}[' + x + ']'
    def stdev_of(x):
        return r'\sigma{}[' + x + ']'
    def mse(x):
        return 'MSE[' + x + ']'
    def cardinality(x):
        return r'\|' + x + r'\|'
    (stations, busy_sum, busy_max, avg_distance, water_level, total_danger, danger1, danger2, danger3, danger4, danger5) = (
        'station-handle', 'station-busy[sum]', 'station-busy[max]',
        'solve[mean]', 'water-level[mean]', 'total-danger[sum]',
        'danger-1[sum]','danger-2[sum]','danger-3[sum]',
        'danger-4[sum]', 'danger-5[sum]',
    )
    labels = {
        stations: Measure(r'alerts managed by stations'),
        busy_sum: Measure(r"stations in action"),
        busy_max : Measure(r"signal handled by stations"),
        avg_distance : Measure(r"distance from signal"),
        water_level: Measure(r"water level", "mm"),
        total_danger : Measure(r"generated alerts", "alerts"),
        danger1 : Measure(r"one signal", "nodes"),
        danger2: Measure(r"two signals", "nodes"),
        danger3: Measure(r"three signals", "nodes"),
        danger4: Measure(r"four signals", "nodes"),
        danger5: Measure(r"five signals", "nodes"),

    }
    def derivativeOrMeasure(variable_name):
        if variable_name.endswith('dt'):
            return labels.get(variable_name[:-2], Measure(variable_name)).derivative()
        return Measure(variable_name)
    def label_for(variable_name):
        return labels.get(variable_name, derivativeOrMeasure(variable_name)).description()
    def labels_for(*name):
        return map(lambda x: label_for(x), name)
    def unit_for(variable_name):
        return str(labels.get(variable_name, derivativeOrMeasure(variable_name)))
    
    # Setup libraries
    np.set_printoptions(formatter={'float': floatPrecision.format})
    # Read the last time the data was processed, reprocess only if new data exists, otherwise just load
    import pickle
    import os
    if os.path.exists(directory):
        newestFileTime = max([os.path.getmtime(directory + '/' + file) for file in os.listdir(directory)], default=0.0)
        try:
            lastTimeProcessed = pickle.load(open('timeprocessed', 'rb'))
        except:
            lastTimeProcessed = -1
        shouldRecompute = not os.path.exists(".skip_data_process") and newestFileTime != lastTimeProcessed
        if not shouldRecompute:
            try:
                means = pickle.load(open(pickleOutput + '_mean', 'rb'))
                stdevs = pickle.load(open(pickleOutput + '_std', 'rb'))
            except:
                shouldRecompute = True
        if shouldRecompute:
            timefun = np.logspace if logarithmicTime else np.linspace
            means = {}
            stdevs = {}
            for experiment in experiments:
                # Collect all files for the experiment of interest
                import fnmatch
                allfiles = filter(lambda file: fnmatch.fnmatch(file, experiment + '_*.csv'), os.listdir(directory))
                allfiles = [directory + '/' + name for name in allfiles]
                allfiles.sort()
                # From the file name, extract the independent variables
                dimensions = {}
                for file in allfiles:
                    dimensions = mergeDicts(dimensions, extractCoordinates(file))
                dimensions = {k: sorted(v) for k, v in dimensions.items()}
                # Add time to the independent variables
                dimensions[timeColumnName] = range(0, timeSamples)
                # Compute the matrix shape
                shape = tuple(len(v) for k, v in dimensions.items())
                # Prepare the Dataset
                dataset = xr.Dataset()
                for k, v in dimensions.items():
                    dataset.coords[k] = v
                if len(allfiles) == 0:
                    print("WARNING: No data for experiment " + experiment)
                    means[experiment] = dataset
                    stdevs[experiment] = xr.Dataset()
                else:
                    varNames = extractVariableNames(allfiles[0])
                    for v in varNames:
                        if v != timeColumnName:
                            novals = np.ndarray(shape)
                            novals.fill(float('nan'))
                            dataset[v] = (dimensions.keys(), novals)
                    # Compute maximum and minimum time, create the resample
                    timeColumn = varNames.index(timeColumnName)
                    allData = { file: np.matrix(openCsv(file)) for file in allfiles }
                    computeMin = minTime is None
                    computeMax = maxTime is None
                    if computeMax:
                        maxTime = float('-inf')
                        for data in allData.values():
                            maxTime = max(maxTime, data[-1, timeColumn])
                    if computeMin:
                        minTime = float('inf')
                        for data in allData.values():
                            minTime = min(minTime, data[0, timeColumn])
                    timeline = timefun(minTime, maxTime, timeSamples)
                    # Resample
                    for file in allData:
    #                    print(file)
                        allData[file] = convert(timeColumn, timeline, allData[file])
                    # Populate the dataset
                    for file, data in allData.items():
                        dataset[timeColumnName] = timeline
                        for idx, v in enumerate(varNames):
                            if v != timeColumnName:
                                darray = dataset[v]
                                experimentVars = extractCoordinates(file)
                                darray.loc[experimentVars] = data[:, idx].A1
                    # Fold the dataset along the seed variables, producing the mean and stdev datasets
                    mergingVariables = [seed for seed in seedVars if seed in dataset.coords]
                    means[experiment] = dataset.mean(dim = mergingVariables, skipna=True)
                    stdevs[experiment] = dataset.std(dim = mergingVariables, skipna=True)
            # Save the datasets
            pickle.dump(means, open(pickleOutput + '_mean', 'wb'), protocol=-1)
            pickle.dump(stdevs, open(pickleOutput + '_std', 'wb'), protocol=-1)
            pickle.dump(newestFileTime, open('timeprocessed', 'wb'))
    else:
        means = { experiment: xr.Dataset() for experiment in experiments }
        stdevs = { experiment: xr.Dataset() for experiment in experiments }

    # QUICK CHARTING

    import matplotlib
    import matplotlib.pyplot as plt
    import matplotlib.cm as cmx
    matplotlib.rcParams.update({'axes.titlesize': 12})
    matplotlib.rcParams.update({'axes.labelsize': 10})
    def make_line_chart(xdata, ydata, title = None, ylabel = None, xlabel = None, colors = None, linewidth = 1, errlinewidth = 0.5, figure_size = (6, 4)):
        fig = plt.figure(figsize = figure_size)
        ax = fig.add_subplot(1, 1, 1)
        ax.set_title(title)
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
#        ax.set_ylim(0)
#        ax.set_xlim(min(xdata), max(xdata))
        index = 0
        for (label, (data, error)) in ydata.items():
#            print(f'plotting {data}\nagainst {xdata}')
            lines = ax.plot(xdata, data, label=label, color=colors(index / (len(ydata) - 1)) if colors else None, linewidth=linewidth)
            index += 1
            if error is not None:
                last_color = lines[-1].get_color()
                ax.plot(xdata, data+error, label=None, color=last_color, linewidth=errlinewidth)
                ax.plot(xdata, data-error, label=None, color=last_color, linewidth=errlinewidth)
        return (fig, ax)
    def generate_all_charts(means, errors = None, basedir=''):
        viable_coords = { coord for coord in means.coords if means[coord].size > 1 }
        for comparison_variable in viable_coords - {timeColumnName}:
            mergeable_variables = viable_coords - {timeColumnName, comparison_variable}
            for current_coordinate in mergeable_variables:
                merge_variables = mergeable_variables - { current_coordinate }
                merge_data_view = means.mean(dim = merge_variables, skipna = True)
                merge_error_view = errors.mean(dim = merge_variables, skipna = True)
                for current_coordinate_value in merge_data_view[current_coordinate].values:
                    beautified_value = beautifyValue(current_coordinate_value)
                    for current_metric in merge_data_view.data_vars:
                        title = f'{label_for(current_metric)} for diverse {label_for(comparison_variable)} when {label_for(current_coordinate)}={beautified_value}'
                        for withErrors in [True, False]:
                            fig, ax = make_line_chart(
                                title = title,
                                xdata = merge_data_view[timeColumnName],
                                xlabel = unit_for(timeColumnName),
                                ylabel = unit_for(current_metric),
                                ydata = {
                                    beautifyValue(label): (
                                        merge_data_view.sel(selector)[current_metric],
                                        merge_error_view.sel(selector)[current_metric] if withErrors else 0
                                    )
                                    for label in merge_data_view[comparison_variable].values
                                    for selector in [{comparison_variable: label, current_coordinate: current_coordinate_value}]
                                },
                            )
                            ax.set_xlim(minTime, maxTime)
                            ax.legend()
                            fig.tight_layout()
                            by_time_output_directory = f'{output_directory}/{basedir}/{comparison_variable}'
                            Path(by_time_output_directory).mkdir(parents=True, exist_ok=True)
                            figname = f'{comparison_variable}_{current_metric}_{current_coordinate}_{beautified_value}{"_err" if withErrors else ""}'
                            for symbol in r".[]\/@:":
                                figname = figname.replace(symbol, '_')
                            fig.savefig(f'{by_time_output_directory}/{figname}.pdf')
                            plt.close(fig)
    for experiment in experiments:
        current_experiment_means = means[experiment]
        current_experiment_errors = stdevs[experiment]
        ### Custom charting
        generate_all_charts(current_experiment_means, current_experiment_errors, basedir = f'{experiment}/all')
        means_pd = current_experiment_means.to_dataframe().rename(columns=lambda x: label_for(x))

        current_experiment_errors = stdevs[experiment].to_dataframe().rename(columns=lambda x: label_for(x))
        means_pd = means_pd.fillna(0)
        Path(f'{output_directory}').mkdir(parents=True, exist_ok=True)
        def normalize_color_map(cmap=plt.cm.viridis, interval=(0.3, 0.9), precision=10, name="mycmap"):
            min_val, max_val = interval
            colors = cmap(np.linspace(min_val, max_val, precision))
            return matplotlib.colors.LinearSegmentedColormap.from_list(name, colors)

        def ax_water_level():
            ax = means_pd[label_for(water_level)].plot(secondary_y=True, lw=1, mark_right = False, color="k", linestyle="--", legend="water level")
            ax.set_ylabel(unit_for("mean water level"))
            return ax

        def finalise_fig(ax, name):
            fig = ax.figure
            fig.tight_layout()
            fig.savefig(f'{output_directory}/{name}.pdf')
            plt.close(fig)

        means_pd[labels_for(total_danger, stations)].plot(title="Generated alerts over time").set_ylabel("alerts")
        finalise_fig(ax_water_level(), "danger-and-managed")

        styles = ['x-','o-','*-', 'd-']
        
        means_pd[labels_for(danger1, danger2, danger3, danger4)]\
            .plot(style=styles, ms=2, lw=1, title="Alerted devices over time").set_ylabel("devices receiving signals (devices)")
        
        finalise_fig(ax_water_level(), "danger-evolution")
        
        ## Risk oracle vs risk evaluated 
        ax = means_pd[['max-risk', 'risk-level[max]']].plot()
        ax.set_ylabel("risk level")
        ax.legend(["Oracle", "Our solution"])
        finalise_fig(ax, "risk-oracle")

sea = []
seaRecompute = False
try:
    sea = pickle.load(open("sea_pickle", 'rb'))
except:
    seaRecompute = True
firestations = json.load(open('src/main/resources/fire-station.json', 'rb'))
firestations = [firestation for firestation in firestations if firestation[0] > 43.6]
def riskMapPlot(riskMap, stations, render):
    global sea, seaRecompute
    print(riskMap['time'])
    filteredPosition = [element for element in stations['position'] if element[0] > 43.6]
    latRisk = [element['lat'] for element in riskMap['level']]
    lonRisk = [element['lon'] for element in riskMap['level']]
    lonRiskStation = [element[1] for element in filteredPosition]
    latRiskStation = [element[0] for element in filteredPosition]
    lonFirestations = [element[1] for element in firestations]
    latFirestations = [element[0] for element in firestations]
    if(seaRecompute):
        bm = Basemap(resolution='f')
        sea = [element for element in riskMap['level'] if not bm.is_land(element['lon'], element['lat'])]
        pickle.dump(sea, open("sea_pickle", 'wb'))
        seaRecompute = False
    sea = [element for element in sea if element['lon'] < -79.14 ]
    latSea = [element['lat'] for element in sea]
    lonSea = [element['lon'] +0.027 for element in sea]
    risk = [element['risk'] for element in riskMap['level']]
    ax = plt.gca()
    ax.scatter(x=lonRisk, y=latRisk, c=risk, alpha=0.7, cmap='viridis_r')
    ax.scatter(x=lonSea, y=latSea, c='black', s=5.0, marker="s")
    ax.scatter(x=lonFirestations, y=latFirestations, c='white', marker="P", s=40.0, edgecolors='grey', linewidths=1.5)
    ax.scatter(x=lonRiskStation, y=latRiskStation, c='white', s=40.0, edgecolors='red', linewidths=1.5)
    render(riskMap['time'])

def storeInFile(time):
    folder = "charts/riskmap/"
    if(not os.path.exists(folder)):
        os.mkdir(folder)
    fig = ax.figure
    fig.tight_layout()
    plt.savefig(folder + str(time) + ".pdf")
    plt.clf()
    plt.cla()
    plt.close()

def renderAll():
    with open('data/riskmap/map-seed-0.0', 'r') as riskmapFile,\
            open('data/firestation/positions-seed-0.0', 'r') as fireStationFile:
        riskMap = json.load(riskmapFile)
        fireStationMap = json.load(fireStationFile)
        #riskAndFirestation = zip(riskMap, fireStationMap)
        #camera = Camera(plt.figure())
        #def storeInVideo(time):
        #    camera.snap()
        #for (riskSnapshot, fireStationSnapshot) in riskAndFirestation:
        #    riskMapPlot(riskSnapshot, fireStationSnapshot, storeInVideo)
        #anim = camera.animate(blit=True)
        #anim.save('charts/riskmap/risks-video.mp4')
        for (riskSnapshot, fireStationSnapshot) in zip(riskMap, fireStationMap):
            riskMapPlot(riskSnapshot, fireStationSnapshot, storeInFile)

renderAll()

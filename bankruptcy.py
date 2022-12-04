import pandas as pd
import numpy as np

# - Company name
# - Flags
# - Last avail. year (independent censoring?)

# - Postcode
# - City
# - Region in country

# - BvD sectors
# - Specialisation

# - Peer group name
# - Peer group size

# - Legal events - Date
# - Legal events - Description
# - Legal events - Type

# - Operating revenue [-5:]
# - Cash flow [-5:]
# - Total assets [-5:]
# - Profit margin [-5:]
# - Number of employees [-5:]

# - Status
# - Status date
# - Local status
# - Local status date


def read_df(path):
    df = pd.read_csv(path, delimiter="\t", encoding="UTF-16 LE")
    df = df[df["Mark"].notna()]
    # df["Status"] = df["Status"].astype(str)
    # df["Status date"] = df["Status date"].astype(str)
    return df


def fill_index(df):
    df["Mark"] = df["Mark"].fillna(method="ffill")
    df = df.set_index("Mark")
    return df


def add_status(df, new_df):
    # Add status to new df
    new_df["status"] = df["Legal situation"]
    replace_map = {"Conclusion of liquidation": "Liquidation",
                   "Reopening liquidation": "Liquidation",
                   "Conclusion of bankruptcy": "Bankruptcy"}
    new_df["status"] = new_df["status"].replace(replace_map)

    # Calculate years to event
    new_df["end_date"] = df["Legal situation date"]
    new_df["start_date"] = df["Date of incorporation"]

    new_df["end_date"] = pd.to_datetime(new_df["end_date"])
    new_df["start_date"] = pd.to_datetime(new_df["start_date"])

    new_df["years_to_event"] = [len(pd.date_range(start=i, end=j, freq="Y"))
                                for i, j in zip(new_df["start_date"],
                                                new_df["end_date"])]
    new_df["years_to_event"] = new_df["years_to_event"].astype(int)

    return new_df


def add_event_indicator(new_df):
    indicator_map = new_df["status"].unique().tolist()
    indicator_map = {i:j for i, j in
                     zip(indicator_map, list(range(len(indicator_map))))}

    new_df["event_indicator"] = [indicator_map[i] for i in new_df["status"]]

    return new_df


def add_mean_pl(df, new_df):
    pl_columns = [i for i in df.columns if "P/L" in i]
    df[pl_columns] = df[pl_columns].replace("n.a.", np.nan)
    new_df["mean_pl"] = df[pl_columns].fillna(0).mean(axis=1)
    new_df["mean_pl"] = new_df["mean_pl"] / 1000
    return new_df


def add_mean_cash_flow(df, new_df):
    cash_flow_columns = [i for i in df.columns if "Cash flow" in i]
    df[cash_flow_columns] = df[cash_flow_columns].replace("n.a.", np.nan)
    new_df["mean_cash_flow"] = df[cash_flow_columns].fillna(0).mean(axis=1)
    return new_df


def add_mean_total_assets(df, new_df):
    total_assets_columns = [i for i in df.columns if "Total assets" in i]
    df[total_assets_columns] = df[total_assets_columns].replace("n.a.", np.nan)
    new_df["mean_total_assets"] = df[total_assets_columns].fillna(0).mean(axis=1)
    new_df["mean_total_assets"] = new_df["mean_total_assets"] / 1000
    return new_df


def add_mean_profit_margin(df, new_df):
    profit_margin_columns = [i for i in df.columns if "Profit margin" in i]
    df[profit_margin_columns] = df[profit_margin_columns].replace("n.a.", np.nan)
    new_df["mean_profit_margin"] = df[profit_margin_columns].fillna(0).mean(axis=1)
    return new_df


def add_mean_n_employees(df, new_df):
    employees_columns = [i for i in df.columns if "employees" in i]
    df[employees_columns] = df[employees_columns].replace("n.a.", np.nan)
    new_df["mean_n_employees"] = df[employees_columns].fillna(0).mean(axis=1)
    return new_df


def add_mean_legal_events(df, new_df):
    new_df["n_legal_events"] = df["Legal events - Date"].str.count("\n")
    new_df["n_legal_events"] = new_df["n_legal_events"].fillna(0)
    new_df["n_legal_events"] = new_df["n_legal_events"] + 1
    return new_df


def add_peer_group_size(df, new_df):
    new_df["peer_group_size"] = df["Peer Group Size"]
    return new_df


def add_legal_form(df, new_df):
    private_list = [
        "Private company with limited liability", "private_company",
        "Private company limited by shares",
        "Starter Private company with limited liability",
        "Limited partnership", "Partnership limited by shares",
        "General partnership", "One-man company with limited liability",
        "Private company with limited liability with a social aim",
        "Private Founding",
        "Professional partnership in the form of a private company with limited liability",
        "Private company"
    ]
    public_list = [
        "Company limited by shares",
        "Public company limited by shares",
        "Public company of another legal form",
        "Public utility Founding",
        "Public institution"
    ]
    cooperative_list = [
        "Co-operative society with limited liability",
        "Co-operative society with limited liability with a social aim",
        "Co-operative society",
        "Co-operative society with unlimited joint and several liability",
        "Co-operative society with limited liability by way of participating interest",
        "Public co-operative society"
    ]
    nonprofit_list = [
        "Non-profit association", "Private non-profit association",
        "Association in charge of missions",
        "International non-profit association",
        "Company limited by shares with a social aim"
    ]
    other_list = [
        "Foreign company whose nature is not described",
        "Provincial government control",
        "Project Association",
        "Autonomous local authority with legal personality",
        "Economic joint venture with Belgian office",
        "European economic joint venture with Belgian office",
        "Public utility founding"
    ]

    categories =  [private_list, public_list, cooperative_list, nonprofit_list,
                   other_list]
    labels = ["private", "public", "cooperative", "nonprofit", "other"]

    remap = {}
    for i, j in zip(categories, labels):
        for k in i:
            remap[k] = j

    df["Legal form"] = df["Legal form"].replace(remap)

    # Create dummies
    new_df = new_df.merge(pd.get_dummies(df["Legal form"]), how="left",
                          left_index=True, right_index=True)

    return new_df


def main():
    df = read_df(path="data/Bel-first_Export_2.txt")
    df = fill_index(df)

    print(df.columns)

    new_df = pd.DataFrame(index=df.index.unique())
    new_df.index = new_df.index.rename("index")

    new_df = add_status(df, new_df)
    new_df = add_event_indicator(new_df)

    new_df = new_df[new_df["status"].isin(
        ["Liquidation", "Bankruptcy", "Merger by absorption"]
    )]

    new_df = add_mean_pl(df, new_df)
    new_df = add_mean_total_assets(df, new_df)
    new_df = add_mean_n_employees(df, new_df)
    new_df = add_legal_form(df, new_df)

    df.to_csv("data/temp_df.csv")
    new_df.to_csv("data/bankruptcy_transformed.csv", index=False)


if __name__=="__main__":
    main()

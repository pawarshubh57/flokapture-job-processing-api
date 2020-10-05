using System.Data.Entity;
using BusinessLayer.DbEntities;
using BusinessLayer.Models;

namespace BusinessLayer.DatabaseContext
{
    public partial class AppDbContext : DbContext
    {
        public AppDbContext() : base("ConnectionStringMySql") { }
    }

    public partial class AppDbContext
    {
        public DbSet<UserMaster> UserMaster { get; set; }
        public DbSet<ProjectMaster> ProjectMaster { get; set; }
        public DbSet<FileTypeExtensionReference> FileTypeExtensionReference { get; set; }
        public DbSet<LanguageMaster> LanguageMaster { get; set; }
        public DbSet<FileMaster> FileMaster { get; set; }
        public DbSet<PrimaryLanguageReference> PrimaryLanguageReference { get; set; }
        public DbSet<StatementReferenceMaster> StatementReferenceMaster { get; set; }
        public DbSet<BaseCommandReference> BaseCommandReference { get; set; }
        public DbSet<ActionWorkflows> ActionWorkflows { get; set; }
        public DbSet<LanguageKeywords> LanguageKeywords { get; set; }
        public DbSet<DeadDataTypes> DeadDataTypes { get; set; }
        public DbSet<AlwaysConsiderClasses> AlwaysConsiderClasses { get; set; }
        public DbSet<ProjectConfigMaster> ProjectConfigMaster { get; set; }
        public DbSet<ConnectivityStepReference> ConnectivityStepReference { get; set; }
        public DbSet<ConnectivityLinkReferece> ConnectivityLinkReferece { get; set; }
        public DbSet<ProjectDocuments> ProjectDocuments { get; set; }
        public DbSet<UserDetails> UserDetails { get; set; }
        public DbSet<RegexPatternMaster> RegexPatternMaster { get; set; }
        public DbSet<WorkflowLinkDetails> WorkflowLinkDetails { get; set; }
        public DbSet<WorkflowNodeDetails> WorkflowNodeDetails { get; set; }
        public DbSet<WorkflowTreeviewTabFirstDetails> WorkflowTreeviewTabFirstDetails { get; set; }
        public DbSet<WorkflowTreeviewSecondTabDetails> WorkflowTreeviewSecondTabDetails { get; set; }
        public DbSet<DecisionChart> DecisionChart { get; set; }
        public DbSet<DataDictionary> DataDictionary { get; set; }
        public DbSet<StatementWithDataDictionary> StatementWithDataDictionary { get; set; }
        public DbSet<UniverseFileMenu> UniverseFileMenu { get; set; }
        public DbSet<DataDependency> DataDependency { get; set; }
        public DbSet<ProjectType> ProjectType { get; set; }
        public DbSet<ViewSourceMaster> ViewSourceMaster { get; set; }
        public DbSet<WorkflowReferences> WorkflowReferences { get; set; }
        public DbSet<FirstTabProgramDetails> FirstTabProgramDetails { get; set; }
        public DbSet<SecondTabProgramDetails> SecondTabProgramDetails { get; set; }
        public DbSet<ActionWorkflowDetails> ActionWorkflowDetails { get; set; }
        public DbSet<SolutionMaster> SolutionMaster { get; set; }
        public DbSet<ProductConfiguration> ProductConfiguration { get; set; }
        public DbSet<SecondTabSubRoutineProgramDetails> SecondTabSubRoutineProgramDetails { get; set; }
        public DbSet<DbCrudActivity> DbCrudActivity { get; set; }
        public DbSet<LanguageKeyword> LanguageKeyword { get; set; }
        public DbSet<CobolVariable> CobolVariable { get; set; }
        public DbSet<ProjectInventory> ProjectInventory { get; set; }
        public DbSet<EntitiesToExclude> EntitiesToExclude { get; set; }
        public DbSet<UniverseDescriptor> UniverseDecriptor { get; set; }
        public DbSet<DataInventory> DataInventory { get; set; }
        public DbSet<CrudActivityReport> CrudActivityReport { get; set; }
        public DbSet<MissingObjects> EntityMissing { get; set; }
        public DbSet<CobolSection> CobolSection { get; set; }
        public DbSet<CrudObjectReferences> CrudObjectReference { get; set; }
        public DbSet<ProjectProcessingStep> ProjectProcessingStep { get; set; }
    }
}
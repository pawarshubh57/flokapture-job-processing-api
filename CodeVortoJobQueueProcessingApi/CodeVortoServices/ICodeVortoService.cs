using System;
using BusinessLayer.BasicRepositories;
using BusinessLayer.DatabaseContext;
using BusinessLayer.DbEntities;
using BusinessLayer.Models;

namespace CodeVortoJobQueueProcessingApi.CodeVortoServices
{
    public interface ICodeVortoService : IDisposable
    {
        BaseRepository<UserMaster> UserMasterRepository { get; }
        BaseRepository<ProjectMaster> ProjectMasterRepository { get; }
        BaseRepository<FileTypeExtensionReference> FileTypeExtensionRepository { get; }
        BaseRepository<FileMaster> FileMasterRepository { get; }
        BaseRepository<FileTypeReference> FileTypeReferenceRepository { get; }
        BaseRepository<PrimaryLanguageReference> PrimaryLanguageReferenceRepository { get; }
        BaseRepository<StatementReferenceMaster> StatementReferenceMasterRepository { get; }
        BaseRepository<BaseCommandReference> BaseCommandReferenceRepository { get; }
        BaseRepository<ActionWorkflows> ActionWorkflowsRepository { get; }
        BaseRepository<ConnectivityStepReference> ConnectivityStepReferenceRepository { get; }
        BaseRepository<ConnectivityLinkReferece> ConnectivityLinkRefereceRepository { get; }
        BaseRepository<DecisionChart> DecisionChartRepository { get; }
        BaseRepository<DataDependency> DataDependencyRepository { get; }
        BaseRepository<ViewSourceMaster> ViewSourceMasterRepository { get; }
        BaseRepository<WorkflowReferences> WorkflowReferencesRepository { get; }
        BaseRepository<WorkflowTreeviewSecondTabDetails> WorkflowTreeviewSecondTabDetailsRepository { get; }
        BaseRepository<WorkflowTreeviewTabFirstDetails> WorkflowTreeviewTabFirstDetailsRepository { get; }
        BaseRepository<SecondTabProgramDetails> SecondTabProgramDetailsRepository { get; }
        BaseRepository<FirstTabProgramDetails> FirstTabProgramDetailsRepository { get; }
        BaseRepository<RegexPatternMaster> RegexPatternMasterRepository { get; }
        BaseRepository<SolutionMaster>SolutionMasterRepository { get; }
        BaseRepository<ProductConfiguration> ProductConfigurationRepository { get; }
        BaseRepository<SecondTabSubRoutineProgramDetails> SecondTabSubRoutineProgramRepository { get; }
        BaseRepository<DbCrudActivity> DbCrudActivityRepository { get; }
        BaseRepository<StatementWithDataDictionary> StatementWithDataDictionaryRepository { get; }
        BaseRepository<ActionWorkflowDetails> ActionWorkflowDetailsRepository { get; }
        BaseRepository<DataDictionary>DataDictionaryRepository { get; }
        AppDbContext AppDbContextRepository { get; }
        BaseRepository<LanguageKeyword> LanguageKeywordRepository { get; }
        BaseRepository<CobolVariable> CobolVariableRepository { get; }
        BaseRepository<UniverseDescriptor> UniverseDescriptorRepository { get; }
        BaseRepository<DataInventory> DataInventoryRepository { get; }
        BaseRepository<MissingObjects> MissingObjectsRepository { get; }
        BaseRepository<CobolSection> CobolSectionRepository { get; }
        BaseRepository<CrudObjectReferences> CrudObjectReferenceRepository { get; }
        BaseRepository<ProjectProcessingStep> ProjectProcessingStepRepository { get; }
    }
}